open Contact

type field = All | Id | FirstName | LastName | Age | Email | Phone

exception Add_Contact_With_Invalid_Data
exception Remove_Using_An_Invalid_Id
exception Remove_Impossible_On_An_Empty_List
exception Replace_Impossible_On_An_Empty_List
exception Replace_Using_An_Invalid_Id
exception Replace_Contact_With_Invalid_Data

module type AGENDA =
  sig
    val addContact     : Contact.contact list -> string * string * int * string * string -> Contact.contact list
    val is_capitalized : string -> string
    val is_uppercased : string -> string
    val is_emailed : string -> bool
    val is_telephoned : string -> bool
    val sort_contact : Contact.contact list -> Contact.contact list
    val compare_contact : Contact.contact -> Contact.contact -> int
    val getContactId   : Contact.contact list -> field -> string -> int
    val look_fname : Contact.contact list -> string -> int
    val look_lname : Contact.contact list -> string -> int
    val look_age : Contact.contact list -> int -> int
    val look_email : Contact.contact list -> string -> int
    val look_tel : Contact.contact list -> string -> int
    val look_id : Contact.contact list -> int -> int
    val look_all : Contact.contact list -> string -> int
    val lookIn : string -> string -> bool
    val removeContact  : Contact.contact list -> int -> Contact.contact list
    val replaceContact : Contact.contact list -> int -> string * string * int * string * string -> Contact.contact list
    val printContacts  : Contact.contact list -> field -> string -> unit
    val find_ctc  : Contact.contact list -> field -> string -> unit
    val print_ctc : Contact.contact list -> int -> unit
    val print_field : string -> int -> int -> unit
  end

module Agenda : AGENDA =
  struct
    let is_capitalized elem =
      if String.length elem == 0 then raise (Add_Contact_With_Invalid_Data)
      else if String.contains elem '-' == false
      then
          String.capitalize (String.trim elem)
      else
        let rec loop str ind =
          if ind + 1 == String.length str then String.capitalize (String.trim str)
          else if (str.[ind] == '-')
          then
            begin
              Bytes.set str (ind + 1) (Char.uppercase str.[ind + 1]);
              loop str (ind + 1)
            end
          else
            loop str (ind + 1)
      in loop elem 0

    let is_uppercased elem =
        if String.length elem == 0 then raise (Add_Contact_With_Invalid_Data);
        let s = String.trim elem in
        String.uppercase s

    let is_emailed elem =
        let size = String.length elem in

        if String.contains elem '@' == false then false else
        let ind = String.index elem '@' in

        if ind == 0 || ind == size then false else
        if String.contains_from elem ind '.' == false then false else
        let dot = String.index_from elem ind '.' in

        if dot == size || dot == ind + 1 then false
        else true

    let is_telephoned elem =
      let size = String.length elem in

      if size != 14 || elem.[0] != '0' then false else
      if  elem.[2] != ' ' || elem.[5] != ' ' || elem.[8] != ' '
       || elem.[11] != ' ' then false else
       let rec loop elem n =
        if n == 14 then true
        else if ((elem.[n] < '0' || elem.[n] > '9') && elem.[n] != ' ') then false
        else loop elem (n + 1)
       in loop elem 0

    let compare_contact a b =
      let fa = Contact.const_f a in
      let la = Contact.const_l a in
      let fb = Contact.const_f b in
      let lb = Contact.const_l b in
      let fc = String.compare fa fb in
      if fc == 0 then String.compare la lb
      else fc

    let sort_contact lst =
      List.sort compare_contact lst

    let addContact lst (a, b, c, d, e) =
      let fname = is_capitalized a in
      let lname = is_uppercased b in
      if c < 0 || c > 120 then raise (Add_Contact_With_Invalid_Data);
      if a == "" || b == "" then raise (Add_Contact_With_Invalid_Data);
      if is_emailed d == false then raise (Add_Contact_With_Invalid_Data);
      if is_telephoned e == false then raise (Add_Contact_With_Invalid_Data);
      let ctc = Contact.const fname lname c d e in
      sort_contact (ctc::lst)

    let lookIn s f =
      if String.length f > String.length s then false else
      let rec loop ind sta s f =
      if sta == String.length f then true else
      if ind == String.length s then false else
      if (s.[ind] == f.[sta]) then loop (ind + 1) (sta + 1) s f else
      loop ((ind - sta) + 1) 0 s f
    in loop 0 0 s f

    let look_fname lst s =
    let size = List.length lst in
      let rec loop ind s tst =
        if ind == size then - 1 else
        let f_name = String.uppercase (Contact.const_f (List.hd tst)) in
        if lookIn f_name s == true then ind
        else loop (ind + 1) (String.uppercase s) (List.tl tst)
      in loop 0 (String.uppercase s) lst

    let look_lname lst s =
      let rec loop ind s lst =
        if ind - 1 > (List.length lst) then - 1 else
        let l_name = String.uppercase (Contact.const_l (List.hd lst)) in
        if lookIn l_name s == true then ind
        else loop (ind + 1) (String.uppercase s) (List.tl lst)
      in loop 0 (String.uppercase s) lst

    let look_age lst s =
      let rec loop ind lst s =
        if ind - 1 > (List.length lst) then - 1 else
        let age = Contact.const_age (List.hd lst) in
        if age == s then ind
        else loop (ind + 1) (List.tl lst) s
      in loop 0 lst s

    let look_email lst s =
      let rec loop ind s lst =
        if ind - 1 > (List.length lst) then - 1 else
        let em = Contact.const_em (List.hd lst) in
        if lookIn em s == true then ind
        else loop (ind + 1) s (List.tl lst)
      in loop 0 s lst

    let look_tel lst s =
      let rec loop ind s lst =
        if ind - 1 > (List.length lst) then - 1 else
        let tel = Contact.const_tel (List.hd lst) in
        if lookIn tel s == true then ind
        else loop (ind + 1) s (List.tl lst)
      in loop 0 s lst

    let look_id lst id =
      if id >= (List.length lst) || id < 0 then - 1
      else id

    let look_all lst s =
        if look_fname lst s != - 1 then look_fname lst s else
        if look_lname lst s != - 1 then look_lname lst s else
        if look_age lst (int_of_string s) != - 1 then look_age lst (int_of_string s) else
        if look_email lst s != - 1 then look_email lst s else
        if look_tel lst s != - 1 then look_tel lst s else
        -1

    let getContactId lst f s =
      if f == FirstName then look_fname lst s
      else if f == LastName then look_lname lst s
      else if f == Age then look_age lst (int_of_string s)
      else if f == Email then look_email lst s
      else if f == Phone then look_tel lst s
      else if f == Id then look_id lst (int_of_string s)
      else if f == All then look_all lst s
      else -1

    let rec print_field s it max =
      if it >= String.length s then print_char ' '
      else print_char (s.[it]);
      if it < max then
      print_field s (it + 1) max

    let print_ctc lst id =
      if id != -1 then
        begin
          print_field (string_of_int id) 0 3;
          print_field (Contact.const_f (List.nth lst id)) 0 15;
          print_field (Contact.const_l (List.nth lst id)) 0 15;
          print_field (string_of_int (Contact.const_age (List.nth lst id))) 0 3;
          print_field (Contact.const_em (List.nth lst id)) 0 31;
          print_field (Contact.const_tel (List.nth lst id)) 0 13
        end

    let find_ctc lst f s =
     if f == FirstName then print_ctc lst (look_fname lst s) else
     if f == LastName then print_ctc lst (look_lname lst s) else
     if f == Age then print_ctc lst (look_age lst (int_of_string s)) else
     if f == Email then print_ctc lst (look_email lst s) else
     if f == Phone then print_ctc lst (look_tel lst s) else
     if f == All then print_ctc lst (look_all lst s)

     let print_ctc_all lst id =
      if id != -1 then
        begin
          print_field (string_of_int id) 0 3;
          print_field (Contact.const_f (List.hd lst)) 0 15;
          print_field (Contact.const_l (List.hd lst)) 0 15;
          print_field (string_of_int (Contact.const_age (List.hd lst))) 0 3;
          print_field (Contact.const_em (List.hd lst)) 0 31;
          print_field (Contact.const_tel (List.hd lst)) 0 13
        end

    let rec printAll lst id =
      if List.length lst != 0 then
      begin
          print_ctc_all lst id;
          print_endline "";
          printAll (List.tl lst) (id + 1)
      end


    let printContacts lst f s =
      if s = "" then printAll lst 0 else
        find_ctc lst f s

    let removeContact lst id =
      if List.length lst == 0 then raise (Remove_Impossible_On_An_Empty_List);
      if id >= (List.length lst) || id < 0 then raise (Remove_Using_An_Invalid_Id);
      let size = List.length lst in
      let rec loop old young ind =
        if ind == size then (List.rev young) else
        let elem = (List.hd old) in
        if ind == id then loop (List.tl old) young (ind + 1)
        else loop (List.tl old) (elem::young) (ind + 1)
      in loop lst [] 0

    let launch_replace lst id ctc =
      let size = (List.length lst) in
      let rec loop old young ind =
        if ind == size then (List.rev young) else
        let elem = (List.hd old) in
        if ind == id then loop (List.tl old) (ctc::young) (ind + 1)
        else loop (List.tl old) (elem::young) (ind + 1)
      in loop lst [] 0

    let replaceContact lst id (a, b, c, d, e) =
      if List.length lst == 0 then raise (Replace_Impossible_On_An_Empty_List);
      if id >= (List.length lst) || id < 0 then raise (Replace_Using_An_Invalid_Id);
      let fname = is_capitalized a in
      let lname = is_uppercased b in
      if c < 0 || c > 120 then raise (Replace_Contact_With_Invalid_Data);
      if a == "" || b == "" then raise (Replace_Contact_With_Invalid_Data);
      if is_emailed d == false then raise (Replace_Contact_With_Invalid_Data);
      if is_telephoned e == false then raise (Replace_Contact_With_Invalid_Data);
      let ctc = Contact.const fname lname c d e in
      launch_replace lst id ctc;
  end
