let random_string ln =
  let rs = Bytes.create ln in
  let ic = open_in "/dev/urandom" in
  really_input ic rs 0 ln ;
  close_in ic ;
  Bytes.unsafe_to_string rs

module Hash = Digestif.SHA1

let unsafe_bool_of_int : int -> bool = Obj.magic

let populate tbl n =
  let rec go = function
    | 0 -> Fmt.pr "Database populated.\n%!"
    | n ->
      let str = random_string (Hash.digest_size + 1) in
      let hash = Hash.of_raw_string (String.sub str 0 Hash.digest_size) in
      let mark = unsafe_bool_of_int (Char.code str.[Hash.digest_size] land 1) in
      Hashtbl.add tbl hash mark ; go (pred n) in
  if n < 0 then Fmt.invalid_arg "populate" ; go n

let tbl =
  let tbl = Hashtbl.create 32 in
  populate tbl 1_000_000 ; tbl

open Core
open Core_bench

module Set = Hashset.Make(struct type t = int let equal a b = (compare : int -> int -> int) a b = 0 let hash x = x end)

let add_in_ewah tbl =
  let ewah = Ewah.make ~allocator:Ewah.allocator in
  let pos = ref 0 in
  fun () ->
    Stdlib.Hashtbl.iter
      (fun _ -> function
         | true -> ignore @@ Ewah.add ewah !pos ; incr pos
         | false -> incr pos)
      tbl

let add_in_hashset tbl =
  let set = Set.create 32 (* XXX(dinosaure): [ewah] starts with [32]. *) in
  let pos = ref 0 in
  fun () ->
    Stdlib.Hashtbl.iter
      (fun _ -> function
         | true -> Set.add set !pos ; incr pos
         | false -> incr pos)
      tbl

let add_once_in_ewah =
  let ewah = Ewah.make ~allocator:Ewah.allocator in
  fun () -> ignore @@ Ewah.add ewah 1

let add_once_in_hashset =
  let set = Set.create 32 in
  fun () -> Set.add set 1

let iter_and_do_nothing_on_ewah =
  let pos = ref 0 in
  let ewah = Ewah.make ~allocator:Ewah.allocator in
  Stdlib.Hashtbl.iter (fun _ -> function
      | true -> ignore @@ Ewah.add ewah !pos ; incr pos
      | false -> incr pos)
    tbl ;
  fun () -> Ewah.each_bit ewah (fun _ () -> ()) ()

let iter_and_do_nothing_on_hashset =
  let pos = ref 0 in
  let set = Hashset.create 32 in
  Stdlib.Hashtbl.iter
    (fun _ -> function
       | true -> Set.add set !pos ; incr pos
       | false -> incr pos)
    tbl ;
  fun () -> Set.iter (fun _ -> ()) set

let test_add_once_in_ewah = Test.create ~name:"ewah.add_once" add_once_in_ewah
let test_add_once_in_hashset = Test.create ~name:"set.add" add_once_in_hashset
let test_add_in_ewah = Test.create ~name:"ewah.add" (add_in_ewah tbl)
let test_add_in_hashset = Test.create ~name:"set.add" (add_in_hashset tbl)
let test_iter_on_ewah = Test.create ~name:"ewah.iter" iter_and_do_nothing_on_ewah
let test_iter_on_hashset = Test.create ~name:"set.iter" iter_and_do_nothing_on_hashset

let command =
  Bench.make_command [ test_add_in_ewah; test_add_in_hashset
                     ; test_add_once_in_ewah; test_add_once_in_hashset
                     ; test_iter_on_ewah; test_iter_on_hashset ]

let () = Command.run command
