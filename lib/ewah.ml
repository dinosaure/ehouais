type buf = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t
type rlw = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array0.t

let _bits_in_word = Sys.word_size - 1 (* GC bit *)
let _size_of_word = Sys.word_size / 8

(* XXX(dinosaure): check it! *)
let min_word : int -> int -> int = min
let max_word : int -> int -> int = max

module RLW = struct
  (* XXX(dinosaure): in comments, 64-bits arch. *)

  let _rlw_running_bits = _size_of_word * 4 (* 32 *)
  let _rlw_literal_bits =
    _size_of_word * 8 - (1 + 1) (* running bit + GC bit *) - _rlw_running_bits (* 30 *)

  let _rlw_largest_running_count = (1 lsl _rlw_running_bits) - 1 (* 0xFFFFFFFF *)
  let _rlw_largest_literal_count = (1 lsl _rlw_literal_bits) - 1 (* 0x3FFFFFFF *)
  let _rlw_largest_running_count_shift = _rlw_largest_running_count lsl 1 (* 0x1FFFFFFFE *)
  let _rlw_running_len_plus_bit = (1 lsl (_rlw_running_bits + 1)) - 1 (* 0x1FFFFFFFF *)

  let get : rlw -> int = fun arr -> Bigarray.Array0.get arr
  let set : rlw -> int -> unit = fun arr v -> Bigarray.Array0.set arr v

  let get_run_bit rlw =
    (get rlw) land 1
  [@@inline]

  let set_run_bit rlw bit =
    let word = get rlw in
    let word = if bit <> 0 then word land (lnot 1) else word lor 1 in
    set rlw word
  [@@inline]

  let set_running_len rlw len =
    let word = get rlw in
    let word = word lor _rlw_largest_running_count_shift in
    let word = word land ((len lsl 1) lor (lnot _rlw_largest_running_count_shift)) in
    set rlw word
  [@@inline]

  let get_running_len rlw =
    let word = get rlw in
    (word lsr 1) land _rlw_largest_running_count
  [@@inline]

  let get_literal_words rlw =
    let word = get rlw in
    word lsr (1 + _rlw_running_bits)
  [@@inline]

  let set_literal_words rlw len =
    let word = get rlw in
    let word = word lor (lnot _rlw_running_len_plus_bit) in
    let word = word land ((len lsl (_rlw_running_bits + 1)) lor _rlw_running_len_plus_bit) in
    set rlw word
  [@@inline]

  let size rlw =
    (get_running_len rlw) + (get_literal_words rlw)
  [@@inline]
end

type t =
  { mutable buffer : buf
  ; mutable buffer_size : int
  ; mutable alloc_size : int
  ; mutable bit_size : int
  ; mutable rlw : rlw }

external ptr_diff : rlw -> buf -> int = "ewah_ptr_diff" [@@noalloc]

let slice : buf -> int -> rlw = fun buf off ->
  Bigarray.Array1.slice buf off [@@inline]

let reallocator : buf -> int -> buf = fun _ size ->
  Bigarray.Array1.create Bigarray.Int Bigarray.c_layout size [@@inline]

let allocator : int -> buf = fun size ->
  Bigarray.Array1.create Bigarray.Int Bigarray.c_layout size [@@inline]

external set : buf -> int -> int -> unit = "%caml_ba_unsafe_set_1"
external get : buf -> int -> int = "%caml_ba_unsafe_ref_1"

let blit src src_off dst dst_off len =
  let a = Bigarray.Array1.sub src src_off len in
  let b = Bigarray.Array1.sub dst dst_off len in
  Bigarray.Array1.blit a b
[@@inline]

let buffer_grow t new_size =
  if t.alloc_size >= new_size then Fmt.invalid_arg "Ewah.buffer_grow: invalid new size." ;
  let rlw_offset = ptr_diff t.rlw t.buffer in
  let offset = rlw_offset / _size_of_word in
  t.alloc_size <- new_size ;
  t.buffer <- reallocator t.buffer new_size ;
  t.rlw <- slice t.buffer offset
[@@inline]

let buffer_push t value =
  if t.buffer_size + 1 >= t.alloc_size then buffer_grow t (t.buffer_size * 3 / 2) ;
  set t.buffer t.buffer_size value ;
  t.buffer_size <- t.buffer_size + 1
[@@inline]

let buffer_push_rlw t value =
  buffer_push t value ;
  let offset = t.buffer_size - 1 in
  t.rlw <- slice t.buffer offset

let clear t =
  t.buffer_size <- 1 ;
  set t.buffer 0 0 ;
  t.bit_size <- 0 ;
  t.rlw <- slice t.buffer 0

let add_empty_words t v n =
  if RLW.get_run_bit t.rlw != v
  && RLW.size t.rlw == 0
  then RLW.set_run_bit t.rlw v
  else if RLW.get_literal_words t.rlw != 0
          || RLW.get_run_bit t.rlw != v
  then begin
    buffer_push_rlw t 0 ;
    if v != 0 then RLW.set_run_bit t.rlw v
  end ;

  let runlen = RLW.get_running_len t.rlw in
  let can_add = min_word n (RLW._rlw_largest_running_count - runlen) in

  RLW.set_running_len t.rlw (runlen + can_add) ;
  let number = ref (n - can_add) in

  while !number >= RLW._rlw_largest_running_count
  do
    buffer_push_rlw t 0 ;
    (* XXX(dinosaure): test_and_set? *)
    if v != 0 then RLW.set_run_bit t.rlw v ;
    RLW.set_running_len t.rlw RLW._rlw_largest_running_count ;
    number := !number - RLW._rlw_largest_literal_count ;
  done ;

  if !number > 0
  then begin
    buffer_push_rlw t 0 ;
    if v != 0 then RLW.set_run_bit t.rlw v ;
    RLW.set_running_len t.rlw !number ;
  end

let add_literal t literal =
  let current_num = RLW.get_literal_words t.rlw in

  if current_num >= RLW._rlw_largest_literal_count
  then begin
    buffer_push_rlw t 0 ;
    RLW.set_literal_words t.rlw 1 ;
    buffer_push t literal
  end else begin
    RLW.set_literal_words t.rlw (current_num + 1) ;
    (* assert (RLW.get_literal_words t.rlw == current_num + 1) ; *)
    buffer_push t literal
  end

exception Break

let add_dirty_words t buffer n negate =
    let number = ref n in
    let buffer_offset = ref 0 in
    let continue = ref true in

    while !continue do
      let literals = RLW.get_literal_words t.rlw in
      let can_add = min_word n (RLW._rlw_largest_literal_count - literals) in

      RLW.set_literal_words t.rlw (literals + can_add) ;

      if t.buffer_size + can_add >= t.alloc_size
      then buffer_grow t ((t.buffer_size + can_add) * 3 / 2) ;

      if negate != 0
      then
        for i = 0 to can_add - 1 do
          set t.buffer t.buffer_size (lnot (get buffer (!buffer_offset + i))) ;
          t.buffer_size <- t.buffer_size + 1
        done
      else begin
        blit t.buffer t.buffer_size buffer !buffer_offset (can_add * _size_of_word) ;
        t.buffer_size <- t.buffer_size + can_add ;
      end ;

      t.bit_size <- t.bit_size + (can_add * _bits_in_word) ;

      if !number - can_add == 0
      then continue := false
      else
        ( buffer_push_rlw t 0
        ; buffer_offset := !buffer_offset + can_add
        ; number := !number - can_add )
    done

let add_empty_word t v =
  let no_literal = RLW.get_literal_words t.rlw == 0 in
  let run_len = RLW.get_running_len t.rlw in

  if no_literal && run_len == 0
  then begin
    RLW.set_run_bit t.rlw v ;
    (* assert (RLW.get_run_bit t.rlw == v) ; *)
  end ;

  if no_literal && RLW.get_run_bit t.rlw == v
     && run_len < RLW._rlw_largest_running_count
  then begin
    RLW.set_running_len t.rlw (run_len + 1)
    (* assert (RLW.get_running_len t.rlw == run_len + 1) *)
  end else begin
    buffer_push_rlw t 0 ;

    (* assert (RLW.get_running_len t.rlw == 0) ; *)
    (* assert (RLW.get_run_bit t.rlw == 0) ; *)
    (* assert (RLW.get_literal_words t.rlw == 0) ; *)

    RLW.set_run_bit t.rlw v ;
    (* assert (RLW.get_run_bit t.rlw == v) ; *)

    RLW.set_running_len t.rlw 1
    (* assert (RLW.get_running_len t.rlw == 1) ; *)
    (* assert (RLW.get_literal_words t.rlw == 0) *)
  end

let add t word =
  t.bit_size <- t.bit_size + _bits_in_word ;
  if word == 0
  then add_empty_word t 0
  else if word == (lnot 0)
  then add_empty_word t 1
  else add_literal t word

let (//) n d = (n + d - 1) / d [@@inline]
let (%) x n = x mod n [@@inline] (* XXX(dinosaure): optimize it! *)

let each_bit t f =
  let pointer = ref 0 in
  let pos = ref 0 in

  while !pointer < t.buffer_size
  do
    let word = slice t.buffer !pointer in

    if RLW.get_run_bit word != 0
    then
      let len = RLW.get_running_len word * _bits_in_word in
      for _ = 0 to len - 1 do f !pos ; incr pos done
    else
      pos := !pos + (RLW.get_running_len word * _bits_in_word) ;

    incr pointer ;

    let len = RLW.get_literal_words word in
    for _ = 0 to len - 1 do
      for c = 0 to _bits_in_word - 1 do
        if get t.buffer !pointer land (1 lsl c) != 0
        then f !pos ;

        incr pos
      done;

      incr pointer
    done
  done

let pp =
  let iter f ewah = each_bit ewah f in
  Fmt.iter iter Fmt.int

type 'a rd = < rd: unit; .. > as 'a

module PRLW : sig
  type 'a t

  type rd_only = < rd: unit; > t
  (* XXX(dinosaure): to prove [const] assertion. *)

  val const : rlw -> rd_only
  val get_literal_words : 'a rd t -> int
  val get_running_len : 'a rd t -> int
  val get_run_bit : 'a rd t -> int
end = struct
  type 'a rd = < rd: unit; .. > as 'a
  type 'a t = rlw

  type rd_only = < rd: unit; > t

  external const : rlw -> rd_only = "%identity"
  let get_literal_words : 'a rd t -> int = fun rlw -> RLW.get_literal_words rlw [@@inline]
  let get_running_len : 'a rd t -> int = fun rlw -> RLW.get_running_len rlw [@@inline]
  let get_run_bit : 'a rd t -> int = fun rlw -> RLW.get_run_bit rlw [@@inline]
end

module PBuf : sig
  type 'a t

  type rd_only = < rd: unit; > t

  val const : buf -> rd_only
  val get : 'a rd t -> int -> int
  val slice : 'a rd t -> int -> rlw
  val unsafe_shift : 'a rd t -> int -> buf
end = struct
  type 'a rd = < rd: unit; .. > as 'a
  type 'a t = buf

  type rd_only = < rd: unit; > t

  external const : buf -> rd_only = "%identity"
  let get : 'a rd t -> int -> int = fun buf off -> get buf off [@@inline]
  let slice : 'a rd t -> int -> rlw = fun buf off -> slice buf off [@@inline]
  let unsafe_shift : 'a rd t -> int -> buf =
    fun buf off ->
      let len = Bigarray.Array1.dim buf in
      Bigarray.Array1.sub buf off (len - off) [@@inline]
end

module Iterator = struct
  type ewah = t

  type t =
    { buffer : PBuf.rd_only
    ; size : int
    ; mutable pointer : int
    ; mutable literal_word_start : int
    ; rlw : uncompressed_rlw }
  and uncompressed_rlw =
    { mutable word : PRLW.rd_only
    ; mutable literal_words : int
    ; mutable running_len : int
    ; mutable literal_word_offset : int
    ; mutable running_bit : int }

  let zero =
    let res = Bigarray.Array0.create Bigarray.Int Bigarray.c_layout in
    RLW.set res 0 (* bzero *); res

  let empty = Bigarray.Array1.create Bigarray.Int Bigarray.c_layout 0

  let default_uncompressed_rlw =
    { word= PRLW.const zero
    ; literal_words= 0
    ; running_len= 0
    ; literal_word_offset= 0
    ; running_bit= 0 }

  let default =
    { buffer= PBuf.const empty
    ; size= 0
    ; pointer= 0
    ; literal_word_start= 0
    ; rlw= default_uncompressed_rlw }

  let word_size t = t.rlw.running_len + t.rlw.literal_words
  let literal_words t = t.pointer - t.rlw.literal_words

  let next t =
    if t.pointer < t.size
    then
      let prlw = PRLW.const @@ PBuf.slice t.buffer t.pointer in
      let literal_words = PRLW.get_literal_words prlw in
      t.rlw.word <- prlw ;
      t.rlw.running_len <- PRLW.get_running_len prlw ;
      t.rlw.running_bit <- PRLW.get_run_bit prlw ;
      t.rlw.literal_words <- literal_words ;
      t.rlw.literal_word_offset <- 0 ;
      t.pointer <- t.pointer + succ literal_words ;
      true
    else false

  let make (ewah : ewah) =
    let res = { default with buffer= PBuf.const ewah.buffer
                           ; size= ewah.buffer_size } in
    ignore @@ next res ;
    res.literal_word_start <- literal_words res + res.rlw.literal_word_offset ;
    res

  exception Return

  let discard_first_words t x =
    let x = ref x in

    try
      while !x > 0
      do
        if t.rlw.running_len > !x
        then ( t.rlw.running_len <- t.rlw.running_len - !x ;
               raise_notrace Return ) ;

        x := !x - t.rlw.running_len ;
        t.rlw.running_len <- 0 ;

        let discard = if !x > t.rlw.literal_words then t.rlw.literal_words else !x in

        t.literal_word_start <- t.literal_word_start + discard ;
        t.rlw.literal_words <- t.rlw.literal_words - discard ;
        x := !x - discard ;

        if !x > 0 || word_size t == 0
        then
          ( if not (next t)
            then raise_notrace Break ;

            t.literal_word_start <- literal_words t + t.rlw.literal_word_offset )
      done
    with
    | Return | Break -> ()

  let discharge t ewah max negate =
    let index = ref 0 in

    while !index < max && word_size t > 0
    do
      let pl = ref t.rlw.running_len in
      if !index + !pl > max then pl := max - !index ;

      add_empty_words ewah (t.rlw.running_bit lxor negate) !pl ;
      index := !index + !pl ;

      let pd = ref t.rlw.literal_words in
      if !pd + !index > max then pd := max - !index ;

      add_dirty_words ewah (PBuf.unsafe_shift t.buffer t.literal_word_start) !pd negate ;
      discard_first_words t (!pd + !pl) ;
      index := !index + !pd ;
    done ; !index
end

exception Invalid_bit

let set t i =
  let dist = ((i + 1) // _bits_in_word) - (t.bit_size // _bits_in_word) in

  if i < t.bit_size then raise Invalid_bit ;

  t.bit_size <- i + 1 ;

  if dist > 0
  then begin
    if dist > 1 then add_empty_words t 0 (dist - 1) ;
    add_literal t (1 lsl (i % _bits_in_word))
  end else if RLW.get_literal_words t.rlw == 0
  then begin
    RLW.set_running_len t.rlw (RLW.get_running_len t.rlw - 1) ;
    add_literal t (1 lsl (i % _bits_in_word))
  end else begin
    set t.buffer (t.buffer_size - 1) (get t.buffer (t.buffer_size - 1) lor (1 lsl (i mod _bits_in_word))) ;
    (* check if we just completed a stream of 1s *)

    if get t.buffer (t.buffer_size - 1) == lnot 0
    then begin
      t.buffer_size <- t.buffer_size - 1 ;
      set t.buffer t.buffer_size 0 ;
      RLW.set_literal_words t.rlw (RLW.get_literal_words t.rlw - 1) ;
      add_empty_word t 1
    end
  end

let add_empty_words t v n =
  if n == 0 then ()
  else
    ( t.bit_size <- t.bit_size + (n * _bits_in_word) ; add_empty_words t v n )

let xor a b o =
  let i = Iterator.make a in
  let j = Iterator.make b in

  while Iterator.word_size i > 0
        && Iterator.word_size j > 0
  do
    while i.Iterator.rlw.running_len > 0 || j.Iterator.rlw.running_len > 0
    do
      let prey, predator =
        if i.Iterator.rlw.running_len < j.Iterator.rlw.running_len
        then i, j else j, i in
      let negate_words = lnot (lnot predator.Iterator.rlw.running_bit) in
      let index = Iterator.discharge prey o predator.Iterator.rlw.running_len negate_words in
      add_empty_words o negate_words (predator.Iterator.rlw.running_len - index) ;
      Iterator.discard_first_words predator predator.Iterator.rlw.running_len
    done ;

    let literals = min_word i.Iterator.rlw.literal_words j.Iterator.rlw.literal_words in

    if literals > 0
    then begin
      for k = 0 to literals - 1 do
        add o (PBuf.get i.Iterator.buffer (i.literal_word_start + k)
               lxor PBuf.get j.Iterator.buffer (j.literal_word_start + k)) ;
      done ;

      Iterator.discard_first_words i literals ;
      Iterator.discard_first_words j literals ;
    end
  done ;

  let _ =
    if Iterator.word_size i > 0
    then Iterator.discharge i o (lnot 0) 0
    else Iterator.discharge j o (lnot 0) 0 in

  o.bit_size <- max_word a.bit_size b.bit_size

let make ~allocator =
  let buffer = allocator 32 in
  let res = { buffer
            ; buffer_size= 1
            ; alloc_size= 32
            ; bit_size= 0
            ; rlw= slice buffer 0 } in
  clear res ; res
