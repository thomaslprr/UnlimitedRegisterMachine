(* Unlimited Register Machine Implementation *)

(*Registers*)

(*Registre = int et un index*)
type register = Vide | Register of int;;
let register = Register(3) ;;

(*Basic Instructions *)

type commands = Zero of int | Successor of int| Copy of int*int | Jump of int*int*int ;;

(*Program*)

type numbercommands = Numbercommands of commands list * int ;;


type program = Program of numbercommands list ;;


(*Operation*)

let execute_commands commands = 
	match commands with
	| Zero(n) -> 
	| Successor(n) ->
	| Copy(m,n) -> 
	| Jump(m,n,q) -> 
;;

(*Termination*)

(*Input*)

(*Output*)

(*Null program*)


