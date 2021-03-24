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
	
	
let nat_to_int n naturalPlus = 	
	let res = let rec aux_nat_to_int n= 
	  match n with
	    | Null   -> 0
	    | Succ m -> 1 + aux_nat_to_int m 
	in aux_nat_to_int n 
in match naturalPlus with
	| true -> if res = 0 then failwith "nat_to_int is undefined for 0 in natural *" else res
	| false -> res
	;;
		
		

let int_to_nat i naturalPlus =
	let res = let rec aux_int_to_nat i =
	  if i < 0 then failwith "int_to_nat is undefined on negative ints"
	  else if i = 0 then Null
	  else Succ (aux_int_to_nat (i-1)) 
  	in  (aux_int_to_nat i) 
in match naturalPlus with
	| true -> if res=Null then failwith "int_to_nat is undefined for 0 in natural *" else res
	| false -> res ;; 


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

let programmevide = Commands([],(int_to_nat 5 true),[Register((int_to_nat 23 false))]) ;;
let programmesansregistre = Commands([],(int_to_nat 1 true),[]) ;;


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
	 
	let m = (nat_to_int m false) in
	let n = (nat_to_int n false) in
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
				let instructioncrt = nat_to_int instructioncourrante false in  
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
		
	
	



