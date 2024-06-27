type variable = string
type symbol = string
type signature = (symbol * int) list
type term = Var of variable | Num of float | Node of symbol * (term list)
type atom = Atom of symbol * (term list)
type head = Head of atom
type body = Body of atom list
type clause = Fact of head | Rule of head * body
type program = clause list
type goal = Goal of atom list
type substitution = (variable * term) list

exception NOT_UNIFIABLE
exception NotFound
exception InvalidProgram
exception NotPossible