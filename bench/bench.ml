open Bechamel
open Toolkit

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
    | 0 -> ()
    | n ->
      let str = random_string (Hash.digest_size + 1) in
      let hash = Hash.of_raw_string (String.sub str 0 Hash.digest_size) in
      let mark = unsafe_bool_of_int (Char.code str.[Hash.digest_size] land 1) in
      Hashtbl.add tbl hash mark ; go (pred n) in
  if n < 0 then Fmt.invalid_arg "populate" ; go n

let tbl =
  let tbl = Hashtbl.create 32 in
  populate tbl 1_000 ; tbl

module Set = Hashset.Make(struct type t = int let equal a b = (compare : int -> int -> int) a b = 0 let hash x = x end)

let add_in_ewah tbl =
  let ewah = Ewah.make ~allocator:Ewah.allocator in
  let pos = ref 0 in
  Staged.stage @@ fun () ->
    Hashtbl.iter
      (fun _ -> function
         | true -> ignore @@ Ewah.set ewah !pos ; incr pos
         | false -> incr pos)
      tbl

let add_in_hashset tbl =
  let set = Set.create 32 (* XXX(dinosaure): [ewah] starts with [32]. *) in
  let pos = ref 0 in
  Staged.stage @@ fun () ->
    Hashtbl.iter
      (fun _ -> function
         | true -> Set.add set !pos ; incr pos
         | false -> incr pos)
      tbl

let test_add_in_ewah = Test.make ~name:"ewah" (add_in_ewah tbl)
let test_add_in_hashset = Test.make ~name:"set" (add_in_hashset tbl)
let test = Test.make_grouped ~name:"add" [ test_add_in_ewah; test_add_in_hashset; ]

let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |] in
  let instances =
    Instance.[ minor_allocated; major_allocated; monotonic_clock ] in
  let cfg =
    Benchmark.cfg ~limit:2000 ~quota:(Time.second 0.5) ~kde:(Some 1000) () in
  let raw_results = Benchmark.all cfg instances test in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  (results, raw_results)

let nothing _ = Ok ()

let () =
  let results = benchmark () in
  let results =
    let open Bechamel_js in
    emit ~dst:(Channel stdout) nothing ~compare:String.compare ~x_label:Measure.run
      ~y_label:(Measure.label Instance.monotonic_clock)
      results in
  match results with Ok () -> () | Error (`Msg err) -> invalid_arg err
