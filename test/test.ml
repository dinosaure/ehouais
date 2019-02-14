let test_set ewah pos reject =
  let name = Fmt.strf "set <ewah> %d" pos in
  Alcotest.test_case name `Quick @@ fun () ->
  if reject
  then Alcotest.check_raises "reject" Ewah.Invalid_bit (fun () -> Ewah.set ewah pos)
  else
    ( Ewah.set ewah pos ;
      let set = ref false in
      Ewah.each_bit ewah (fun pos' -> if pos = pos' then set := true) ;
      Alcotest.(check bool) "pass" !set true )

let test_empty ewah is_empty =
  let name = Fmt.strf "is_empty: %b" is_empty in
  Alcotest.test_case name `Quick @@ fun () ->
  if is_empty
  then
    ( let is_empty = ref true in
      Ewah.each_bit ewah (fun _ -> is_empty := false) ;
      Alcotest.(check bool) "is_empty" !is_empty true )
  else
    ( let is_empty = ref true in
      Ewah.each_bit ewah (fun _ -> is_empty := false) ;
      Alcotest.(check bool) "is_empty" !is_empty false )

let test_not_on_singleton n =
  let name = Fmt.strf "not singleton %d" n in
  Alcotest.test_case name `Quick @@ fun () ->
  let ewah = Ewah.make ~allocator:Ewah.allocator in
  Ewah.set ewah n ;
  Ewah.compute_not ewah ;
  let exists = ref false in
  Ewah.each_bit ewah (fun pos -> if pos = n then exists := true) ;
  Alcotest.(check bool) "not exists" !exists false

module Set = Set.Make(struct type t = int let compare a b = a - b end)

let ewah_fold ewah f a =
  let a = ref a in
  Ewah.each_bit ewah (fun x -> a := f x !a) ; !a

let xor a b =
  Set.(diff (union a b) (inter a b))

let test_xor a b =
  let name = Fmt.strf "xor %a %a"
      Fmt.(Dump.list int) (Set.elements a)
      Fmt.(Dump.list int) (Set.elements b) in
  Alcotest.test_case name `Quick @@ fun () ->
  let ewah_a = Ewah.make ~allocator:Ewah.allocator in
  let ewah_b = Ewah.make ~allocator:Ewah.allocator in
  Set.iter (fun x -> Ewah.set ewah_a x) a ;
  Set.iter (fun x -> Ewah.set ewah_b x) b ;
  let ewah_o = Ewah.make ~allocator:Ewah.allocator in
  Ewah.compute_xor ewah_a ewah_b ewah_o ;
  let o = xor a b in
  let res = ewah_fold ewah_o (fun pos acc -> Set.mem pos o && acc) true in
  let len = ewah_fold ewah_o (fun _ acc -> succ acc) 0 in
  Alcotest.(check bool) "xor" res true ;
  Alcotest.(check int) "len" (Set.cardinal o) len

let singleton x =
  let ewah = Ewah.make ~allocator:Ewah.allocator in
  Ewah.set ewah x ; ewah

let set_operation =
  [ 1, false
  ; 76, false
  ; 77, false
  ; 8712800127, false
  ; 25, true ]

let set_operation () =
  let ewah = Ewah.make ~allocator:Ewah.allocator in
  List.map (fun (pos, reject) -> test_set ewah pos reject) set_operation

let empty_operation =
  [ Ewah.make ~allocator:Ewah.allocator, true
  ; singleton 42, false ]

let empty_operation =
  List.map (fun (ewah, is_empty) -> test_empty ewah is_empty) empty_operation

let not_operation = [ 0; 1; 42 ]

let not_operation =
  List.map test_not_on_singleton not_operation

let xor_operation =
  [ [ 1; 2; 3 ], [ 2 ]
  ; [ 1; 2; 3 ], [ 4; 5; 6; ]
  ; [ 1; 2; 3; 4 ], [ 1; 3; 5 ] ]

let set_of_list lst = List.fold_right Set.add lst Set.empty

let xor_operation =
  List.map (fun (a, b) -> test_xor (set_of_list a) (set_of_list b)) xor_operation

let () =
  Alcotest.run "ewah"
    [ "set", set_operation ()
    ; "empty", empty_operation
    ; "not", not_operation
    ; "xor", xor_operation ]
