module type CONTACT =
  sig
    type contact

    val const : string -> string -> int -> string -> string -> contact
    val const_f : contact -> string
    val const_l : contact -> string
    val const_age : contact -> int
    val const_tel : contact -> string
    val const_em : contact -> string
  end

module Contact : CONTACT
