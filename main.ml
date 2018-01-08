open Agenda

let main() =
  let name = Agenda.is_capitalized "    arthur-test  " in
  begin
    Printf.printf "%s\n" name;
    Agenda.addContact [] ("zdd", "eeiei", 34, "test@lol.fr", "04 66 83 21 86")
  end

  let _ = main()
