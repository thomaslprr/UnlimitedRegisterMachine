(* Unlimited Register Machine Implementation *)


(*Import pour l'utilisation de printf*)
open Format ;;


type natural = Null | Succ of natural ;;

let iszero n = 
  match n with
    | Null   -> true
    | Succ m -> false ;;

let pred n = 
  match n with
    | Null   -> failwith "pred Zero is undefined"
    | Succ m -> m ;;
	
	let rec nat_to_int n plus= 
	  match n with
	    | Null   -> if(plus=false) then 0  else failwith "Value 0 doesn't exist for natural * "
	    | Succ m -> 1 + nat_to_int m plus;;

	let rec int_to_nat i plus=
  	if i < 0 then failwith "nat_of_int is undefined on negative ints"
	else if (i=0) then
	  match plus with
	  | true -> failwith "Value 0 doesn't exist for natural * "
	  | false -> Null
	  
	 else match (i,plus) with 
	 | (1,true) -> Succ Null
	 | (_,_) -> Succ (int_to_nat (i-1) plus) ;;


(*Register / Registre *)

(*Création du type registre 
- Prend un int en paramètre qui correspond à la valeur que prendra le registre 

Exemple d'appel : # let register1 = Register(342) 
La valeur de register1 sera de 342*)
type register = Register of natural ;;


(*Basic Instructions / Instructions *)

type command = Zero of natural | Successor of natural| Copy of natural*natural | Jump of natural*natural*natural ;;

(*Program / Programme *)

type program = Commands of command list * natural * register list ;;


(*Exemple de création de programme *)

let programmevide = Commands([],1,[Register(23)]) ;;
let programmesansregistre = Commands([],1,[Register(23)]) ;;


(* Programme qui additionne deux registres*)
let programme1 = Commands([Jump((int_to_nat 2 true),(int_to_nat 3 true),(int_to_nat 5 false));Successor(int_to_nat 3 true);Successor((int_to_nat 1 true));Jump((int_to_nat 1 true),(int_to_nat 1 true),(int_to_nat 1 false))],(int_to_nat 1 true),[Register((int_to_nat 12 false));Register((int_to_nat 6 false));Register((int_to_nat 0 false))]);; 
(*Programme qui soustrait deux registres*)
let programme2 = Commands([Jump(1,2,5);Successor(-3);Successor(0);Jump(0,1,1);Copy(-3,1)],1,[Register(25);Register(11);Register(0)]);; 
(*Programme qui effectue une division par 2 et renvoie la valeur si c'est possible sinon boucle infinie*)
let programme3 = Commands([Jump(1,2,6);Successor(3);Successor(2);Successor(2);Jump(1,1,1);Copy(3,1)],1,[Register(245);Register(2);Register(1)]);; 


(*Gestion de l'affichage*)
let print_registre listeRegistre = 
	let rec aux_print_registre listeRegistre compteur= match listeRegistre with
		[] -> ();
		| e::l -> match e with 
		 			| Register(n) -> printf "R%d=%d " compteur (nat_to_int n false); aux_print_registre l (compteur+1)
	in aux_print_registre listeRegistre 1
;;
	
let print_instruction instruction = 
	match instruction with
	| Zero(n) -> printf "Instruction Zero(%d) : " (nat_to_int n true);
	| Successor(n) -> printf "Instruction Successor(%d) : " (nat_to_int n true);
	| Copy(m,n) -> printf "Instruction Copy(%d,%d) : " (nat_to_int m true) (nat_to_int n true);
	| Jump(m,n,q) -> printf "Instruction Jump(%d,%d,%d) : " (nat_to_int m true) (nat_to_int n true) (nat_to_int q false);
;;

(*Fonctions parallèles*)

let jump liste m n q instructioncourrante = 
	 
	let m = (nat_to_int m true) in
	let n = (nat_to_int n true) in
	if (m<(List.length liste) && n<(List.length liste)) then 
										match (List.nth liste m, List.nth liste n) with 
											| Register(x),Register(y) -> if x=y then q else Succ(instructioncourrante)
										else
											Succ(instructioncourrante);;
			
	
let replace liste position newvalue =
	let position = nat_to_int position false in 
	List.mapi (fun i x -> if i=position then newvalue else x) liste ;;
		 
let add liste position = 
	let position = nat_to_int position false in 
	List.mapi (fun i x -> if i=position then 
							match x with 
							| Register(z) -> Register(Succ z) 
						else x) liste ;;



(*Execute commands / Execution d'instructions*)
						
let execute_commands program =
	match program with
		| Commands(listeDesCommandes,instructioncourrante,listeDesRegistres) -> 
			print_registre listeDesRegistres;
			print_newline();
			let rec aux_execute_commands liste listecommandes instructioncourrante = 
				let instructioncrt = nat_to_int instructioncourrante true in
				if (listecommandes != []) then 
				begin
				print_instruction (List.nth listecommandes instructioncrt) ;
				print_newline();
					let listeregistre = match (List.nth listecommandes instructioncrt) with
					| Zero(n) -> replace liste (pred n) (Register(Null))
					| Successor(n) -> add liste (pred n)
					| Copy(m,n) -> 
						let mm = nat_to_int m true in
						let nn = nat_to_int n true in
						if (mm<=List.length liste && nn<=List.length liste) then replace liste (pred n) (List.nth liste (pred m)) else liste
					| Jump(m,n,q) -> liste 
	
					in let instruction = match (List.nth listecommandes instructioncrt) with
								| Jump(m,n,q) -> jump liste (pred m) (pred n) (pred q) instructioncrt
								| _ -> Succ(instructioncrt)
							in 
   							print_registre listeregistre;
 							print_newline();
							if (nat_to_int instruction false) < List.length(listecommandes) then
								 aux_execute_commands listeregistre listecommandes instruction
							 else
								 (List.hd listeregistre)
					end
				else
					begin
					printf "Programme vide";
					print_newline();
					(List.hd liste)	
				end	 
	
			in aux_execute_commands listeDesRegistres listeDesCommandes (pred instructioncourrante)
		
;;




let execute_commands program =
	match program with
		| Commands(listeDesCommandes,instructioncourrante,listeDesRegistres) -> 
			print_registre listeDesRegistres;
			print_newline();
			let rec aux_execute_commands liste listecommandes instructioncourrante = 
				let instructioncrt = nat_to_int instructioncourrante true in  
				if (listecommandes != []) then 
				begin
				print_instruction (List.nth listecommandes instructioncrt) ;
				print_newline();
					let listeregistre = match (List.nth listecommandes instructioncrt) with
					| Zero(n) -> replace liste (pred n) (Register(Null))
					| Successor(n) -> add liste (pred n)
					| Copy(m,n) -> 
						let mm = nat_to_int m true in
						let nn = nat_to_int n true in
						if (mm<=List.length liste && nn<=List.length liste) then replace liste (pred n) (List.nth liste (mm-1)) else liste
						| Jump(m,n,q) -> liste 
						
					
					in 

					let instruction = match (List.nth listecommandes instructioncrt) with
													| Jump(m,n,q) -> jump liste (pred m) (pred n) (pred q) instructioncourrante
													| _ -> Succ(instructioncourrante)
												in 
												print_registre listeregistre;
												print_newline();
												if (nat_to_int instruction false) < List.length(listecommandes) then
													 aux_execute_commands listeregistre listecommandes instruction
												 else
								 (List.hd listeregistre)
					end
				else
					begin
					printf "Programme vide";
					print_newline();
					(List.hd liste)	
				end	 
	
			in aux_execute_commands listeDesRegistres listeDesCommandes (pred instructioncourrante)
		
;;
		
	
	



