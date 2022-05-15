module type Hang = sig
  (*Hangman function takes 2 argument String : filename, int : according line for word*)
  val hangman : string -> int -> unit
end
module Hangman : Hang = struct 
  let check word input genWord = 
    let rec check' index =
      if index = (String.length word) then 
        "" 
      else if word.[index] = input.[0] then (String.make 1 word.[index]) ^ check' (index + 1) 
      else (String.make 1 genWord.[index]) ^ check' (index + 1) 
    in check' 0;;
  let exists cr word =
    let len = String.length word in
    let rec ex index =
      if index = len then false
      else if word.[index] = cr.[0] then true else
        ex (index + 1)
      in ex (0);;
  let print str = print_string str;;

  let read_file filename =
    let lines = ref [] in
    let chan = open_in filename in
    try
      while true; do
        lines := input_line chan :: !lines
      done; !lines
    with End_of_file ->
      close_in chan;
      List.rev !lines ;;

  let get_word filename lineNumb = 
    let file = read_file filename in
    let len = List.length file in 
    if
      lineNumb <= 0 || lineNumb > len then
    failwith "wrong line Input"
    else
      List.nth file (lineNumb - 1);; 
  
  (*filename is "test"*)
  let hangman filename lineNumb = 
    let word = get_word filename lineNumb in
    let dashes = ((String.make (String.length word) '_')) in let () = print_endline dashes in 
    let rec play attempts result = 
      if word = result then print_string ("You won word was indeed: " ^ word)
      else if attempts = 0 then print_string ("you Have lost word was: " ^ word)
      else let input = read_line () in let res = check word input result in let () = print_endline res in if(exists input word) then play (attempts) res
      else play(attempts - 1) res 
    in play 7 dashes;;
end