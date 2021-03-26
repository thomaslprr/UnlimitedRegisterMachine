(* Unlimited Register Machine Implementation *)


(*Import pour l'utilisation de printf*)
open Format ;;

(*Déclaration type entier naturel*)
type natural = Null | Succ of natural ;;
(* Fonction pour savoir si un entier naturel est égal à 0 *)
let iszero n = 
  match n with
    | Null   -> true
    | Succ m -> false ;;

(*Fonction qui retourne le prédécesseur d'un entier naturel et fail s'il n'en existe pas*)
let pred n = 
  match n with
    | Null   -> failwith "pred Zero is undefined"
    | Succ m -> m ;;
	
(*Fonction qui convertit un entier naturel en entier*)	
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
		
		
(*Fonction qui convertit un int en entier naturel et fail si ce n'est pas possible*)
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
- Prend un entier naturel en paramètre qui correspond à la valeur que prendra le registre 

Exemple d'appel : # let register1 = Register(int_to_nat 342 false) 
La valeur de register1 sera de 342*)
type register = Register of natural ;;


(*Basic Instructions / Instructions *)

type command = Zero of natural | Successor of natural| Copy of natural*natural | Jump of natural*natural*natural ;;

(*Program / Programme *)

type program = Commands of command list * natural * register list ;;

(*Import des exemples*)
#use "examples.ml" ;;

(*Gestion de l'affichage*)
(*affiche tous les registres d'une liste et leur valeur*)
let print_registre listeRegistre = 
	let rec aux_print_registre listeRegistre compteur= match listeRegistre with
		[] -> ();
		| e::l -> match e with 
		 			| Register(n) -> printf "R%d=%d " compteur (nat_to_int n false); aux_print_registre l (compteur+1)
	in aux_print_registre listeRegistre 1
;;
(*affiche une instruction*)	
let print_instruction instruction = 
	match instruction with
	| Zero(n) -> printf "Instruction Zero(%d) : " (nat_to_int n true);
	| Successor(n) -> printf "Instruction Successor(%d) : " (nat_to_int n true);
	| Copy(m,n) -> printf "Instruction Copy(%d,%d) : " (nat_to_int m true) (nat_to_int n true);
	| Jump(m,n,q) -> printf "Instruction Jump(%d,%d,%d) : " (nat_to_int m true) (nat_to_int n true) (nat_to_int q false);
;;

(*Fonctions parallèles/intermédiaires*)
(*retourne l'instruction à exécuter après un saut*)
let jump listeregistres m n q instructioncourrante = 
	 
	let m = (nat_to_int (pred m) false) in
	let n = (nat_to_int (pred n) false) in
	if (m<(List.length listeregistres) && n<(List.length listeregistres)) then 
										match (List.nth listeregistres m, List.nth listeregistres n) with 
											| Register(x),Register(y) -> if x=y then (pred q) else Succ(instructioncourrante)
										else
											Succ(instructioncourrante);;
			
(*Fonction qui remplace une valeur par une autre *)	
let replace listeregistres position newvalue =
	let position = nat_to_int position false in 
	List.mapi (fun i x -> if i=position then newvalue else x) listeregistres ;;

(*Fonction qui ajoute un à une position précisée*)
let add listeregistres position = 
	let position = nat_to_int position false in 
	List.mapi (fun i x -> if i=position then 
							match x with 
							| Register(z) -> Register(Succ z) 
						else x) listeregistres ;;



(*Execute commands / Execution d'instructions*)
(*Fonction qui retourne une liste de registre après avoir subi une instruction*)
let getRegisters listeregistres listecommandes instructioncourrante = 
	match (List.nth listecommandes instructioncourrante) with
	| Zero(n) -> replace listeregistres (pred n) (Register(Null))
	| Successor(n) -> add listeregistres (pred n)
	| Copy(m,n) -> 
		if ((nat_to_int m true)<=List.length listeregistres && (nat_to_int n true)<=List.length listeregistres) 
			then replace listeregistres (pred n) (List.nth listeregistres ((nat_to_int (pred m) false))) 
			else listeregistres
	| Jump(m,n,q) -> listeregistres  ;;
	
(*Fonction qui retourne la valeur de la prochaine instruction à exécuter après avoir subi une instruction*)
let getInstruction listecommandes instructioncourrante listeregistres =	
	match (List.nth listecommandes instructioncourrante) with
							| Jump(m,n,q) -> jump listeregistres m n q (int_to_nat instructioncourrante false)
							| _ -> Succ((int_to_nat instructioncourrante false)) ;;

(* Fonction principale qui prend un programme en paramètre et renvoie sa liste de registre(s) avec leur valeur obtenue à la fin de l'exécution *)
let execute_commands program =
	match program with
		| Commands(listeDesCommandes,instructioncourrante,listeDesRegistres) -> 
				(*Affichage des registres au début du programme*)
				print_registre listeDesRegistres;
				print_newline();
				(*On vérifie que le programme contient des registres et qu'il ne s'agit pas d'un programme vide*)
				match (listeDesCommandes, listeDesRegistres) with
				| (_,[]) -> failwith "Le programme ne contient aucun registre"
				| ([],_) -> printf "Programme vide";
							print_newline();
							(List.hd listeDesRegistres)
				| (_,_) ->	
				
				(*Sous programme récursif*)
				let rec aux_execute_commands listeregistres listecommandes instructioncourrante = 
						
						
							(*Affichage de l'instruction en cours*)
							print_instruction (List.nth listecommandes instructioncourrante) ;
							print_newline();
							
							(*Récupération de l'état des registres après exécution de l'instruction*)
							let liste_registres_maj = getRegisters listeregistres listecommandes instructioncourrante in 
								(*Récupération de l'état des instructions après l'exécution de l'instruction*)
								let instruction = getInstruction listecommandes instructioncourrante listeregistres in 
									(*Affichage des registres après une instruction*)
									print_registre liste_registres_maj;
									print_newline();
												
									(*S'il y a encore des instructions on continue le programme sinon on arrête*)
									if (nat_to_int instruction false) < List.length(listecommandes) then
										aux_execute_commands liste_registres_maj listecommandes (nat_to_int instruction false)
									else
								 		(List.hd liste_registres_maj)
	
			in aux_execute_commands listeDesRegistres listeDesCommandes (nat_to_int (pred instructioncourrante) false) ;;
		
	
	
