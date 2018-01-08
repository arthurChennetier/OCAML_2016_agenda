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

module Contact : CONTACT  =
  struct
    type contact = (string * string * int * string * string)

    let const a b c d e = (a, b, c, d, e)
    let const_f (a, b, c, d, e) = a
    let const_l (_, b, _, _, _) = b
    let const_age (_, _, c, _, _) = c
    let const_em (_, _, _, d, _) = d
    let const_tel (_, _, _, _, e) = e
  end
