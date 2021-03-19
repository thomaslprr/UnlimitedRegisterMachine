(* Unlimited Register Machine Implementation *)


(*Registre = int et un index*)
type register = Register of int | Empty ;;



(*Basic Instructions *)

type command = Zero of int | Successor of int| Copy of int*int | Jump of int*int*int ;;

(*Program*)

type program = Commands of command list * int * register list | Vide ;;


(*Test liste*)
let register1 = Register(1) ;;
let register2 = Register(2) ;;
let liste = [register1;register2] ;;
let programme = Commands([Zero(0);Zero(1);Successor(0);Successor(0);Copy(0,1)],0,[Register(12);Register(18)]);;
let programme1 = Commands([Zero(0)],0,[Register(12)]);;


match programme with
	| Commands(listeDesCommandes,instructioncourrante,listeDesRegistres) -> match (List.nth listeDesCommandes instructioncourrante) with Zero(n) -> replace listeDesRegistres n (Register(0))
;;

(*Operation*)
let execute_commands program =
	match program with
		| Commands(listeDesCommandes,instructioncourrante,listeDesRegistres) -> 
			let rec aux_execute_commands liste listecommandes instructioncourrante = 
					let listeregistre = match (List.nth listecommandes instructioncourrante) with
					| Zero(n) -> replace liste n (Register(0))
					| Successor(n) -> add liste n
					| Copy(m,n) -> replace liste n (List.nth liste m)
					| Jump(m,n,q) -> liste 
	
					in let instruction = match (List.nth listecommandes instructioncourrante) with
								| Jump(m,n,q) -> jump liste m n q instructioncourrante
								| _ -> instructioncourrante+1
							in if instruction <= List.length(listecommandes) then
								 aux_execute_commands listeregistre listecommandes instruction
							 else
								 (List.hd listeregistre)
	
			in aux_execute_commands listeDesRegistres listeDesCommandes instructioncourrante
		| Vide -> failwith("Programme vide")
		
;;

let jump liste m n q instructioncourrante = match (List.nth liste m, List.nth liste n) with 
						| Register(x),Register(y) -> if x=y then q else instructioncourrante+1
						| _ -> instructioncourrante+1;;
		
		
	
let replace liste position newvalue =
	List.mapi (fun i x -> if i=position then newvalue else x) liste ;;
		 
let add liste position = 
	List.mapi (fun i x -> if i=position then 
							match x with 
							| Register(z) -> Register(z+1) 
							| _ -> x
						else x) liste ;;
		
		
	

(*Termination*)

(*Input*)

(*Output*)

(*Null program*)


