(* Unlimited Register Machine Implementation *)


open Format ;;


(*Registre = int et un index*)
type register = Register of int | Empty ;;



(*Basic Instructions *)

type command = Zero of int | Successor of int| Copy of int*int | Jump of int*int*int ;;

(*Program*)

type program = Commands of command list * int * register list | Vide ;;


(*Test liste*)


let programme3 = Commands([Successor(0);Jump(0,1,99);Jump(0,0,0)],0,[Register(0);Register(100);]);; (*affichage des entiers positifs de 1 à 100*)
let programme3 = Commands([Jump(1,2,-1);Successor(2);Successor(0);Jump(0,0,0)],0,[Register(12);Register(6);Register(0)]);; (*Addition de deux registres*)
let programme4 = Commands([Jump(0,1,4);Successor(1);Successor(2);Jump(0,0,0);Copy(2,0)],0,[Register(25);Register(25);Register(0)]);; (*Soustraction de deux registres*)






(*Operation*)
let execute_commands program =
	match program with
		| Commands(listeDesCommandes,instructioncourrante,listeDesRegistres) -> 
			print_registre listeDesRegistres;
			print_newline();
			let rec aux_execute_commands liste listecommandes instructioncourrante = 
				print_instruction (List.nth listecommandes instructioncourrante) ;
				print_newline();
					let listeregistre = match (List.nth listecommandes instructioncourrante) with
					| Zero(n) -> replace liste n (Register(0))
					| Successor(n) -> add liste n
					| Copy(m,n) -> if (m<List.length liste && n<List.length liste && n>=0 && m>=0) then replace liste n (List.nth liste m) else liste
					| Jump(m,n,q) -> liste 
	
					in let instruction = match (List.nth listecommandes instructioncourrante) with
								| Jump(m,n,q) -> jump liste m n q instructioncourrante
								| _ -> instructioncourrante+1
							in 
   							print_registre listeregistre;
 							print_newline();
							if instruction < List.length(listecommandes) && instruction>=0 then
								 aux_execute_commands listeregistre listecommandes instruction
							 else
								 (List.hd listeregistre)
							 
	
			in aux_execute_commands listeDesRegistres listeDesCommandes instructioncourrante
		| Vide -> failwith("Programme vide")
		
;;


let jump liste m n q instructioncourrante = if (m<(List.length liste) && n<(List.length liste)) then 
										match (List.nth liste m, List.nth liste n) with 
											| Register(x),Register(y) -> if x=y then q else instructioncourrante+1
											| _ -> instructioncourrante+1
										else
											instructioncourrante+1;;
			
	
let replace liste position newvalue =
	List.mapi (fun i x -> if i=position then newvalue else x) liste ;;
		 
let add liste position = 
	List.mapi (fun i x -> if i=position then 
							match x with 
							| Register(z) -> Register(z+1) 
							| _ -> x
						else x) liste ;;


(*Gestion de l'affichage*)
let print_registre listeRegistre = 
	let rec aux_print_registre listeRegistre compteur= match listeRegistre with
		[] -> ();
		| e::l -> match e with 
		 			| Register(n) -> printf "R%d=%d " compteur n; aux_print_registre l (compteur+1)
					| _ -> printf "R%d=Vide " compteur; aux_print_registre l (compteur+1) 
	in aux_print_registre listeRegistre 0
;;
	
let print_instruction instruction = 
	match instruction with
	| Zero(n) -> printf "Instruction Zero(%d) : " n;
	| Successor(n) -> printf "Instruction Successor(%d) : " n;
	| Copy(m,n) -> printf "Instruction Copy(%d,%d) : " m n;
	| Jump(m,n,q) -> printf "Instruction Jump(%d,%d,%d) : " m n q;
;;
		
	



