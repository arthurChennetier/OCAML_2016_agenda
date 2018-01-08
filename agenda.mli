open Contact

type field = All | Id | FirstName | LastName | Age | Email | Phone

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

module Agenda : AGENDA
