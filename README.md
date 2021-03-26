# UnlimitedRegisterMachine
Implementation of Unlimited Register Machine with OCaml

# Fichiers

## unlimitedRegisterMachine

Ce fichier contient toute l'implémentation du modèle de calcul avec la définition des types, des fonctions intermédiaires et de la fonction principale. Ce fichier inclut *examples.ml*.

## examples

Ce fichier contient plusieurs exemples d'utilisation des machines à registres. 

# Compilation

Pour utiliser le module développer il faut : 
- Se placer à la base du répertoire contenant les deux fichiers .ml
- Lancer ocaml : 
``> eval `opam env` `` puis ``> ocaml `` 
- Importer le code source principal :
 ``> #use "unlimitedRegisterMachine.ml";;``

Vous devriez voir tout le script défiler et s'exécuter.

# Exécution

A présent, si vous souhaitez tester le code et découvrir le module développé, plusieurs jeux d'essais ont été conçu. Vous retrouverez ci-dessous les différents exemples, leur rôle, comment les utiliser et les modifier.

### empty_program

Cet exemple permet de montrer comment réagis la machine à registre lors d'un programme vide. 

Exécution : `> execute_commands empty_program ;;`

### program_without_register
Cet exemple permet de montrer comment réagis la machine à registre lors d'un programme sans aucun registre spécifié. 

Exécution : `> execute_commands program_without_register ;;`
### addition_program
Cet exemple est un programme permettant de montrer l'exécution d'une addition par le biais d'une machine à registre. 

Par défaut : r1=12, r2=6 

Exécution : `> execute_commands addition_program ;;`
### subtraction_program
Cet exemple est un programme permettant de montrer l'exécution d'une soustraction par le biais d'une machine à registre. 

Par défaut : r1=25, r2=15 

Exécution : `> execute_commands subtraction_program ;;`
### divisional_program
Cet exemple est un programme permettant de montrer l'exécution d'une division par deux par le biais d'une machine à registre. 
**Attention :** si le nombre r1 n'est pas divisible par deux alors boucle infinie !

Par défaut : r1=246

Exécution : `> execute_commands divisional_program ;;`
### multiplication_program
Cet exemple est un programme permettant de montrer l'exécution d'une multiplication par le biais d'une machine à registre. 

Par défaut : r1=201, r2=4 

Exécution : `> execute_commands multiplication_program ;;`


> **Note:** Pour exécuter les exemples vous devez avoir importé le fichier principal "unlimitedRegisterMachine.ml" et être en mode "OCaml" sur votre console.

# Liens utiles pour comprendre le projet

- [Définition des machines à registres](https://proofwiki.org/wiki/Definition:Unlimited_Register_Machine)

