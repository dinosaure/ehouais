let () =
  let ewah = Ewah.make ~allocator:Ewah.allocator in
  Ewah.set ewah 0xbfdca26de10b44e ;
  Ewah.set ewah 0x19e7a2ef53b9a9d9 ;
  Ewah.set ewah 0x1b451817cbb8a768 ;
  Ewah.each_bit ewah (Format.printf "> %d.\n%!")
