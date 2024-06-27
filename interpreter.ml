open List;;
open String;;
open Ast;;

type abhiram = V of string | C of {node: string * int; children: abhiram list};;

let zero = C {node = ("0", 0); children = []};;
let one = C {node = ("1", 0); children = []};;
let x = V "x";;
let y = V "y";;
let z = V "z";;
let plus_zero_one = C {node = ("+", 2); children = [zero; one]};;
let times_one_x = C {node = ("*", 2); children = [one; x]};;
let plus_zero_y = C {node = ("+", 2); children = [zero; y]};;
let plus_timesonex_pluszeroy =
  C {node = ("+", 2); children = [times_one_x; plus_zero_y]};;
let plus_timesonex_z =
  C {node = ("+", 2); children = [times_one_x; z]};;


(* Helper function to convert a symbol to string *)
let string_of_symbol (s, n) = s ^ "(" ^ string_of_int n ^ ")";;

(* Helper function to convert a tree to string *)
let rec string_of_tree t =
  match t with
  | V x -> x
  | C {node = (s, _); children} ->
    if List.length children = 0 then s
    else s ^ "(" ^ String.concat ", " (List.map string_of_tree children) ^ ")";;

(* Helper function to convert a substitution to string *)
let string_of_substitution s =
  "{" ^ String.concat ", " (List.map (fun (x, t) -> x ^ " -> " ^ string_of_tree t) s) ^ "}";;

(* Helper function to convert a signature to string *)
let string_of_signature sigy =
  String.concat ", " (List.map string_of_symbol sigy);;

(* Function to check if a tree is well-formed *)
  let rec wftree sigy t =
    let rec find_arity sym sig_remaining =
      match sig_remaining with
      | [] -> raise (Failure "Symbol not found")
      | (s, n) :: rest -> if s = sym then n else find_arity sym rest
    in
  
    let rec check_valid_signature sym arity sigy =
      match sigy with
      | [] -> false
      | (s, n) :: rest -> s = sym && n = arity || check_valid_signature sym arity rest
    in
  
    match t with
    | V x -> true
    | C {node = (s, n); children} ->
      let arity = find_arity s sigy in
      n = List.length children && check_valid_signature s n sigy &&
      List.for_all (wftree sigy) children
    ;;  
(* Functions to calculate height, size, and variables *)
let rec ht t =
  match t with
  | V x -> 0
  | C {node; children} -> 1 + List.fold_left max 0 (List.map ht children);;

let rec size t =
  match t with
  V x -> 1
  | C {node; children} -> 1 + List.fold_left (+) 0 (List.map size children);;

(* Function to mirror a tree *)
let rec mirror t =
  match t with
  V x -> V x
  | C {node; children} -> C {node; children = List.map mirror (List.rev children)};;
  
(* ----------------------------------------------------------------------- *)

let rec check_symbol x y = match y with
    [] -> false
  | z::ys -> if x = z then true else check_symbol x ys
;;

let combine l1 l2 =
  List.fold_left2 (fun acc x y -> (x, y) :: acc) [] l1 l2 |> List.rev;;

let rec union l1 l2 =
  let rec aux l1 acc = match l1 with
    | [] -> acc
    | x::xs -> 
      if check_symbol x acc then 
        aux xs acc
      else 
        aux xs (acc @ [x])
  in aux l1 l2;;

let rec check_program prog = match prog with
  | [] -> true
  | Fact(Head(Atom("equal", _)))::xs
  | Fact(Head(Atom("not_equal", _)))::xs
  | Fact(Head(Atom("cut", _)))::xs
  | Fact(Head(Atom(">", _)))::xs
  | Fact(Head(Atom("<", _)))::xs
  | Fact(Head(Atom("<=", _)))::xs
  | Fact(Head(Atom(">=", _)))::xs
  | Rule(Head(Atom("equal", _)), _)::xs
  | Rule(Head(Atom("not_equal", _)), _)::xs
  | Rule(Head(Atom("cut", _)), _)::xs
  | Rule(Head(Atom(">", _)), _)::xs
  | Rule(Head(Atom("<", _)), _)::xs 
  | Rule(Head(Atom("<=", _)), _)::xs
  | Rule(Head(Atom(">=", _)), _)::xs -> 
      raise InvalidProgram
  | _::xs -> 
      check_program xs;;

let rec change_term num t = match t with
    Var(v) -> Var((string_of_int num) ^ v)
  | Node(s, l) -> Node(s, List.map (change_term num) l)
  | _ -> t
;;

let rec change_atom num a =
  let applyChanges (s, terms) = Atom(s, List.map (fun term -> change_term num term) terms) in
  match a with
  | Atom(name, termList) -> applyChanges (name, termList)
;;

let rec change_clause cl num =
  let transformHead atom = Head(change_atom num atom) in
  let transformBody list = Body(List.map (change_atom num) list) in
  match cl with
  | Fact(h) ->
    let updatedHead = transformHead (match h with Head(a) -> a) in
    Fact(updatedHead)
  | Rule(h, b) ->
    let updatedHead = transformHead (match h with Head(a) -> a) in
    let updatedBody = transformBody (match b with Body(l) -> l) in
    Rule(updatedHead, updatedBody)
;;

let rec change_prog program_list iteration_number =
  match program_list with
  | [] -> []
  | clause::rest_of_program ->
      let updated_clause = change_clause clause iteration_number in
      updated_clause :: change_prog rest_of_program (iteration_number + 1)
;;

let rec change_prog_2 program_list target_atom =
  let Atom(target_symbol, _) = target_atom in
  match program_list with
  | [] -> []
  | clause::rest ->
      (match clause with
       | Fact(Head(Atom(symbol, _))) | Rule(Head(Atom(symbol, _)), _) when symbol = target_symbol ->
           change_clause clause 0 :: change_prog_2 rest target_atom
       | _ ->
           clause :: change_prog_2 rest target_atom)
;;

let rec term_variable t =
  match t with
      Var(v) -> [v]
    | Node(s, l) -> List.fold_left union [] (List.map term_variable l)
    | _ -> []
;;

let atom_variable atom =
  match atom with
  | Atom(symbol, terms) -> term_variable (Node(symbol, terms))

let rec goal_variable goal =
  match goal with
  | Goal(atoms) ->
      let variables_from_atoms = List.map atom_variable atoms in
      List.fold_left union [] variables_from_atoms

let rec subst substitution term =
  let apply_subst_to_list terms = List.map (subst substitution) terms in
  match term with
  | Node(node_name, terms) -> Node(node_name, apply_subst_to_list terms)
  | Num(_) -> term
  | Var(variable) ->
    let rec find_substitution subs = 
      match subs with
      | [] -> term
      | (var, value)::rest ->
          if var = variable then value
          else find_substitution rest
    in
    find_substitution substitution
;;

let rec atom_subst substitution (atom: atom) =
  match atom with
  | Atom(symbol, terms) ->
      let substituted_terms = List.map (fun term -> subst substitution term) terms in
      Atom(symbol, substituted_terms)
;;

let rec variable_in_term v t =
  match t with
      Var(x) -> x = v
    | Node(s, l) -> List.fold_left (||) false (List.map (variable_in_term v) l)
    | _ -> false
;;

let compose substitutions1 substitutions2 =
  let apply_substitution pair = 
    let var, term = pair in
    (var, subst substitutions2 term)
  in
  let updated_substitutions1 = List.map apply_substitution substitutions1 in
  updated_substitutions1 @ substitutions2
;;

let rec mgu_term t1 t2 =
  match (t1, t2) with
  | Var(x), Var(y) when x = y -> []
  | Var(x), Var(y) -> [(x, Var(y))]
  | Var(x), Node(_, _) when variable_in_term x t2 -> raise NOT_UNIFIABLE
  | Var(x), _ -> [(x, t2)]
  | Node(_, _), Var(y) when variable_in_term y t1 -> raise NOT_UNIFIABLE
  | Node(_, _), Var(y) -> [(y, t1)]
  | Num(n1), Num(n2) when n1 = n2 -> []
  | Num(_), Num(_) -> raise NOT_UNIFIABLE
  | Num(_), Var(x) -> [(x, t1)]
  | Node(s1, l1), Node(s2, l2) when s1 = s2 && List.length l1 = List.length l2 ->
      List.fold_left (fun acc (term1, term2) -> 
          compose acc (mgu_term (subst acc term1) (subst acc term2))
      ) [] (combine l1 l2)
  | Node(s1, l1), Node(s2, l2) -> raise NOT_UNIFIABLE
  | _ -> raise NOT_UNIFIABLE;;

let mgu_atom atom1 atom2 =
  match (atom1, atom2) with
  | (Atom(symbol1, terms1), Atom(symbol2, terms2)) ->
      mgu_term (Node(symbol1, terms1)) (Node(symbol2, terms2))
;;

let format_float n =
  let str = Printf.sprintf "%.12g" n in
  if String.contains str '.' then
    let trimmed = Str.global_replace (Str.regexp "[0]+$") "" str in
    if String.ends_with ~suffix:"." trimmed then trimmed ^ "0"
    else trimmed
  else
    str;;

let rec term_list_printer tl = match tl with
    [] -> Printf.printf ""
  | [t] -> term_printer t
  | t::tls -> (
      term_printer t;
      Printf.printf ", ";
      term_list_printer tls;
    )

and list_body_printer t = match t with
    Node("empty", []) -> Printf.printf ""
  | Node("list", [t1; Node("empty", [])]) -> term_printer t1
  | Node("list", [t1; t2]) -> (
      term_printer t1;
      Printf.printf ", ";
      list_body_printer t2;
    )
  | _ -> raise NotPossible

and term_printer t = match t with
    Var(v) -> Printf.printf " %s " v
  | Node("empty", []) -> Printf.printf " [] "
  | Node(s, []) -> Printf.printf " %s " s
  | Node("list", _) -> (
      Printf.printf " [";
      list_body_printer t;
      Printf.printf "] ";
    )
  | Node(s, l) -> (
      Printf.printf " %s ( " s;
      term_list_printer l;
      Printf.printf " ) ";
    )
  | Num(n) ->
    let formatted = format_float n in
    Printf.printf " %s " formatted
;;

let rec find_substitution var substitutions =
  match substitutions with
  | [] -> raise NotFound
  | (v, term)::rest -> if v = var then (v, term) else find_substitution var rest

let rec resolve_substitutions substitutions variables =
  match variables with
  | [] -> []
  | var::rest ->
      try
        let found_substitution = find_substitution var substitutions in
        found_substitution :: resolve_substitutions substitutions rest
      with
      | NotFound -> resolve_substitutions substitutions rest

let get_solution unif vars = resolve_substitutions unif vars;;

let rec printSolution substitution = 
  match substitution with
    | [] -> Printf.printf "true."
    | _ ->
        let printPair (var, term) =
          Printf.printf "%s = " var;
          term_printer term;
          Printf.printf ", "
        in
        List.iter printPair substitution;
        Printf.printf "\b\b. "
;;

let atom_is_atom first_atom second_atom substitutions =
  let substituted_first = atom_subst substitutions first_atom in
  let substituted_second = atom_subst substitutions second_atom in
  compose substitutions (mgu_atom substituted_first substituted_second)
;;

let term_is_term first_term second_term substitutions =
  let substituted_first = subst substitutions first_term in
  let substituted_second = subst substitutions second_term in
  compose substitutions (mgu_term substituted_first substituted_second)
;;

let rec simplify t = match t with
    Num(_) -> t
  | Node(op, [t1; t2]) as node ->
      let simplify_binary op_func =
        match (simplify t1, simplify t2) with
          | (Num(n1), Num(n2)) -> Num(op_func n1 n2)
          | _ -> raise NOT_UNIFIABLE
      in
      begin match op with
        | "+" -> simplify_binary ( +. )
        | "-" -> simplify_binary ( -. )
        | "*" -> simplify_binary ( *. )
        | "/" -> simplify_binary ( /. )
        | _ -> node
      end
  | _ -> t;;

let evaluate_numerical_comparison simplified_term1 simplified_term2 comparison_op unif =
  match (simplified_term1, simplified_term2) with
    | (Num(n1), Num(n2)) -> if comparison_op n1 n2 then unif else raise NOT_UNIFIABLE
    | _ -> raise NOT_UNIFIABLE;;

let evaluation a unif =
  match a with
    | Atom("equal", [t1; t2]) | Atom("not_equal", [t1; t2]) ->
        compose unif (mgu_term (simplify (subst unif t1)) (simplify (subst unif t2)))
    | Atom(">", [t1; t2]) ->
        evaluate_numerical_comparison (simplify (subst unif t1)) (simplify (subst unif t2)) (>) unif
    | Atom("<", [t1; t2]) ->
        evaluate_numerical_comparison (simplify (subst unif t1)) (simplify (subst unif t2)) (<) unif
    | Atom("<=", [t1; t2]) ->
        evaluate_numerical_comparison (simplify (subst unif t1)) (simplify (subst unif t2)) (<=) unif
    | Atom(">=", [t1; t2]) ->
        evaluate_numerical_comparison (simplify (subst unif t1)) (simplify (subst unif t2)) (>=) unif
    | _ -> unif;;

let rec goal_solve prog g unif vars =
  match g with
      Goal([]) -> (
        printSolution (get_solution unif vars);
        print_string "\nEnter '.' to stop for this command or ';' to continue to next command: ";
        match read_line() with
        | "." -> (true, [])
        | ";" -> (false, [])
        | _ -> 
          print_endline "Unknown action. Please enter '.' to stop or ';' to continue.";
          goal_solve prog (Goal([])) unif vars
      )
    | Goal(a::gs) -> match a with
          Atom("equal", _) | Atom(">", _) | Atom("<", _) | Atom("<=", _) | Atom(">=", _) -> (
            try goal_solve prog (Goal(gs)) (evaluation a unif) vars
            with NOT_UNIFIABLE -> (false, [])
          )
        | Atom("not_equal", _) -> (
            try (false, evaluation a unif)
            with NOT_UNIFIABLE -> goal_solve prog (Goal(gs)) unif vars
          )
        | Atom("cut", _) -> let _ = goal_solve prog (Goal(gs)) unif vars in (true, [])
        | _ ->
          let new_prog = change_prog_2 prog a in
          let rec iter prog' = match prog' with
              [] -> (false, [])
            | cl::ps -> match cl with
                Fact(Head(a')) -> (
                  try
                    let u = atom_is_atom a' a unif in
                    match goal_solve new_prog (Goal(gs)) u vars with
                        (true, u') -> (true, u')
                      | _ -> iter ps
                  with NOT_UNIFIABLE -> iter ps
                )
              | Rule(Head(a'), Body(al)) -> (
                  try
                    let u = atom_is_atom a' a unif in
                    match goal_solve new_prog (Goal(al @ gs)) u vars with
                        (true, u') -> (true, u')
                      | _ -> iter ps
                  with NOT_UNIFIABLE -> iter ps
                )
        in iter prog
;;

let goal_interprete program goal =
  let initial_substitutions = []
  and goal_vars = goal_variable goal in
  goal_solve program goal initial_substitutions goal_vars
;;

