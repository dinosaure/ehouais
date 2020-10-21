type t =
  | Arr of int array * int * int
  | App of t * t * int * int

let empty = Arr ([||], 0, 0)

let length = function
  | Arr (_, _, len) -> len
  | App (_, _, len, _) -> len

let height = function
  | Arr _ -> 0
  | App (_, _, _, height) -> height

external ( < ) : 'a -> 'a -> bool = "%lessthan"
let ( < ) (x : int) (y : int) = x < y [@@inline]
let max (x : int) (y : int) = if x < y then y else x

let singleton v = Arr ([| v |], 0, 1)
let of_array arr = Arr (arr, 0, Array.length arr)

external append : int array -> int -> int -> int array -> int -> int -> int array =
  "caml_array_append_sub"
  "caml_array_append_sub"

let app = function
  | Arr (_, _, 0), t | t, Arr (_, _, 0) -> t
  | Arr (a1, off1, len1), Arr (a2, off2, len2)
    when len1 + len2 <= 256 ->
    Arr (append a1 off1 len1 a2 off2 len2, 0, len1 + len2)
  | App (t1, Arr (a1, off1, len1), _, _), Arr (a2, off2, len2)
    when len1 + len2 <= 256 ->
    App (t1, Arr (append a1 off1 len1 a2 off2 len2, 0, len1 + len2),
         length t1 + len1 + len2,
         1 + height t1)
  | Arr (a1, off1, len1), App (Arr (a2, off2, len2), t2, _, _)
    when len1 + len2 <= 256 ->
    App (Arr (append a1 off1 len1 a2 off2 len2, 0, len1 + len2),
         t2, len1 + len2 + length t2, 1 + height t2)
  | t1, t2 ->
    App (t1, t2, length t1 + length t2, 1 + max (height t1) (height t2))

let append t1 t2 = app (t1, t2)

let rec unsafe_get t i = match t with
  | Arr (a, off, _) ->
    Array.unsafe_get a (off + i)
  | App (t1, t2, _, _) ->
    let n1 = length t1 in
    if i < n1
    then unsafe_get t1 i
    else unsafe_get t2 (i - n1)

let get t i =
  if i < 0 || i >= length t then invalid_arg "index out of bounds" ;
  unsafe_get t i

let rec set i v = function
  | Arr (a, off, _) ->
    Array.set a (off + i) v
  | App (t1, t2, _, _) ->
    let n1 = length t1 in
    if i < n1
    then set i v t1
    else set (i - n1) v t2

let set t i v =
  let n = length t in
  if i < 0 || i >= n then invalid_arg "index out of bounds" ;
  set i v t

let rec blit src src_off dst dst_off len =
  match src with
  | Arr (a, off, len) ->
    Array.blit a (off + src_off) dst dst_off len
  | App (t1, t2, _, _) ->
    let n1 = length t1 in
    let roff = src_off - n1 in
    if roff >= 0 then blit t2 roff dst dst_off len
    else
      let llen = - roff in
      if len <= llen
      then blit t1 src_off dst dst_off len
      else ( blit t1 src_off dst dst_off llen
           ; blit t2 0 dst (dst_off + llen) (len - llen) )

let blit src src_off dst dst_off len =
  if len < 0 || src_off < 0 || src_off > length src - len
   || dst_off < 0 || dst_off > Array.length dst - len
  then invalid_arg "blit" ;
  blit src src_off dst dst_off len

module Cursor = struct
  type path = Top | Left of path * t | Right of t * path

  type cursor =
    { rpos : int
    ; loff : int
    ; leaf : t
    ; path : path }

  let rec unzip t = function
    | Top -> t
    | Left (p, tr) -> unzip (app (t, tr)) p
    | Right (tl, p) -> unzip (app (tl, t)) p

  let to_ropes cursor = unzip cursor.leaf cursor.path

  let make ropes i =
    let rec zip loff p = function
      | Arr (_, _, _) as leaf ->
        { rpos= i - loff; loff; leaf= leaf; path= p }
      | App (t1, t2, _, _) ->
        let n1 = length t1 in
        if i < loff + n1
        then zip loff (Left (p, t2)) t1
        else zip (loff + n1) (Right (t1, p)) t2 in
    if i < 0 || i > length ropes then invalid_arg "index out of bounds" ;
    zip 0 Top ropes

  let get cursor = match cursor.leaf with
    | Arr (a, off, len) ->
      let i = cursor.rpos in
      if i = len then invalid_arg "index out of bounds" ;
      Array.unsafe_get a (off + i)
    | App _ -> assert false
end
