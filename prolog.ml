open Lexer;;
open Parser;;
open Interpreter;;

let input = open_in Sys.argv.(1);;
let code = Parser.program Lexer.token (Lexing.from_channel input);;
let temp = check_program code;;
let program = change_prog code 1;;

print_string "Program loaded now please cotinue with your commands.\n";;

while(true) do
  print_string "?- ";
  let command = read_line() in
  if command = "stop." then exit 0
  else try
    let goal = Parser.goal Lexer.token (Lexing.from_string command) in
    match (goal_interprete program goal) with
        (true, _) -> print_string "true.\n"
      | (false, _) -> print_string "false.\n"
  with e -> Printf.printf "%s\n" (Printexc.to_string e)
done