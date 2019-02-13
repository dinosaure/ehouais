let test_set ewah pos reject =
  let name = Fmt.strf "set <ewah> %d" pos in
  Alcotest.test_case name `Quick @@ fun () ->
  if reject
  then Alcotest.check_raises "reject" Ewah.Invalid_bit (fun () -> Ewah.set ewah pos)
  else
    ( Ewah.set ewah pos ;
      let set = ref false in
      Ewah.each_bit ewah (fun pos' () -> if pos = pos' then set := true) () ;
      Alcotest.(check bool) "pass" !set true )

let set_operation =
  [ 1, false
  ; 76, false
  ; 77, false
  ; 8712800127, false
  ; 25, true ]

let set_operation () =
  let ewah = Ewah.make ~allocator:Ewah.allocator in
  List.map (fun (pos, reject) -> test_set ewah pos reject) set_operation

let () =
  Alcotest.run "ewah"
    [ "set operation", set_operation () ]
