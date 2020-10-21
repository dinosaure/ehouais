open Crowbar

let random_ewah =
  map [ list int ] @@ fun bitv ->
  let ewah = Ewah.make ~allocator:Ewah.allocator in
  List.iter (fun v -> Ewah.set ewah (abs v)) bitv ; ewah

module Set = Hashset.Make(struct
    type t = int

    let equal a b = (compare : int -> int -> int) a b = 0
    let hash x = x
end)

let xor u v =
  let r = Set.create 32 in
  Set.iter (fun u -> if not (Set.mem v u) then Set.add r u) u ;
  Set.iter (fun v -> if not (Set.mem u v) then Set.add r v) v ;
  r

let () = add_test ~name:"each_bit" [ list int ] @@ fun bitv ->
  let bitv = List.map abs bitv in
  let bitv = List.sort Int.compare bitv in
  let ewah = Ewah.make ~allocator:Ewah.allocator in
  List.iter (Ewah.set ewah) bitv ;
  let check = ref true in
  Ewah.each_bit ewah (fun v -> check := List.exists ((=) v) bitv && !check) ;
  if not !check then failf "Invalid ewah"

