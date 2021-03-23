(* Unlimited Register Machine Implementation *)


(*Import pour l'utilisation de printf*)
open Format ;;


(*Register / Registre *)

(*Création du type registre 
- Prend un int en paramètre qui correspond à la valeur que prendra le registre 

Exemple d'appel : # let register1 = Register(342) 
La valeur de register1 sera de 342*)
type register = Register of int ;;


(*Basic Instructions / Instructions *)

type command = Zero of int | Successor of int| Copy of int*int | Jump of int*int*int ;;

(*Program / Programme *)

type program = Commands of command list * int * register list | Vide ;;


(*Exemple de création de programme *)

(* Programme qui additionne deux registres*)
let programme1 = Commands([Jump(2,3,5);Successor(3);Successor(1);Jump(1,1,1)],1,[Register(12);Register(6);Register(0)]);; 
(*Programme qui soustrait deux registres*)
let programme2 = Commands([Jump(1,2,5);Successor(-3);Successor(0);Jump(0,1,1);Copy(-3,1)],1,[Register(25);Register(11);Register(0)]);; 
(*Programme qui effectue une division par 2 et renvoie la valeur si c'est possible sinon boucle infinie*)
let programme3 = Commands([Jump(1,2,6);Successor(3);Successor(2);Successor(2);Jump(1,1,1);Copy(3,1)],1,[Register(245);Register(2);Register(1)]);; 


(*Gestion de l'affichage*)
let print_registre listeRegistre = 
	let rec aux_print_registre listeRegistre compteur= match listeRegistre with
		[] -> ();
		| e::l -> match e with 
		 			| Register(n) -> printf "R%d=%d " compteur n; aux_print_registre l (compteur+1)
	in aux_print_registre listeRegistre 1
;;
	
let print_instruction instruction = 
	match instruction with
	| Zero(n) -> printf "Instruction Zero(%d) : " n;
	| Successor(n) -> printf "Instruction Successor(%d) : " n;
	| Copy(m,n) -> printf "Instruction Copy(%d,%d) : " m n;
	| Jump(m,n,q) -> printf "Instruction Jump(%d,%d,%d) : " m n q;
;;

(*Fonctions parallèles*)

let jump liste m n q instructioncourrante = if (m<(List.length liste) && n<(List.length liste) && n>=0 && m>=0) then 
										match (List.nth liste m, List.nth liste n) with 
											| Register(x),Register(y) -> if x=y then q else instructioncourrante+1
										else
											instructioncourrante+1;;
			
	
let replace liste position newvalue =
	List.mapi (fun i x -> if i=position then newvalue else x) liste ;;
		 
let add liste position = 
	List.mapi (fun i x -> if i=position then 
							match x with 
							| Register(z) -> Register(z+1) 
						else x) liste ;;



(*Execute commands / Execution d'instructions*)
						
let execute_commands program =
	match program with
		| Commands(listeDesCommandes,instructioncourrante,listeDesRegistres) -> 
			print_registre listeDesRegistres;
			print_newline();
			let rec aux_execute_commands liste listecommandes instructioncourrante = 
				print_instruction (List.nth listecommandes instructioncourrante) ;
				print_newline();
					let listeregistre = match (List.nth listecommandes instructioncourrante) with
					| Zero(n) -> replace liste (n-1) (Register(0))
					| Successor(n) -> add liste (n-1)
					| Copy(m,n) -> if (m<=List.length liste && n<=List.length liste && n>0 && m>0) then replace liste (n-1) (List.nth liste (m-1)) else liste
					| Jump(m,n,q) -> liste 
	
					in let instruction = match (List.nth listecommandes instructioncourrante) with
								| Jump(m,n,q) -> jump liste (m-1) (n-1) (q-1) instructioncourrante
								| _ -> instructioncourrante+1
							in 
   							print_registre listeregistre;
 							print_newline();
							if instruction < List.length(listecommandes) && instruction>=0 then
								 aux_execute_commands listeregistre listecommandes instruction
							 else
								 (List.hd listeregistre)
							 
	
			in aux_execute_commands listeDesRegistres listeDesCommandes (instructioncourrante-1)
		| Vide -> failwith("Programme vide")
		
;;





		
	



