

type 'a elem = { mutable prev : 'a _elem ;
                 mutable next : 'a _elem ;
                 info : 'a               }
and 'a _elem =
    | FVide
    | Elem of 'a elem ;;

type 'a _file = { mutable premier : 'a _elem ;
                  mutable dernier : 'a _elem  } ;;

(* initialise une DeQueue vide *)
let init () = {premier = FVide ; dernier = FVide} ;;


let get_premier file =
    file.premier ;;

let get_dernier file =
    file.dernier ;;

(* 
    val insere_file : 'a -> 'a file -> unit = <fun> 
    insere v dans la file
*)
let insere v f =
    match f.premier with
    | FVide -> let rec elem = Elem {prev = FVide ; next = FVide ; info = v} in
                    f.premier <- elem ; f.dernier <- elem 
    | _     -> let ndernier = Elem {prev = f.dernier ; next = FVide ; info = v} in
               match f.dernier with
               | FVide  -> failwith "problème, file.premier <> Fvide alors que file.dernier = FVide\n"
               | Elem x -> x.next <- ndernier ; f.dernier <- ndernier ;;

(* 
    'a _file -> unit = <fun> 
    retire de la file l'element en tete de file
*)
let enleve_tete f =
    match f.premier with
    | FVide -> failwith "La file est vide, rien a faire\n"
    | Elem x -> let npremier = x.next in
                match npremier with
                | FVide -> f.premier <- FVide ; f.dernier <- FVide
                | Elem y -> let _ = y.prev <- FVide in
                            f.premier <- npremier ;; 

(* 
    val retirer_elem_file : 'a -> 'a _file -> 'a _elem -> unit = <fun> 
    retire l'element elem de la file
*)
let rec retirer_elem a file elem =
    match elem with 
    | FVide -> failwith "La file est vide, rien a faire (1)\n"
    | Elem x -> ( let rec aux = function
                        | FVide -> failwith "La file est vide, rien a faire (2)\n"
                        | Elem x -> if x.info = a
                                    then (  match x.prev, x.next with
                                            | FVide, FVide -> file.premier <- FVide ; file.dernier <- FVide
                                            | FVide, (Elem suivant_x) -> file.premier <- x.next ; suivant_x.prev <- FVide
                                            | (Elem precedent_x), FVide -> file.dernier <- x.prev ; precedent_x.next <- FVide
                                            | (Elem precedent_x), (Elem suivant_x) -> let _ = precedent_x.next <- x.next in
                                                                    suivant_x.prev <- x.prev ) 
                                    else (aux x.next) 
                    in 
                        (aux elem)) ;;

(*
    val get_sommet_file : 'a _file -> 'a = <fun>
    renvoie l'élément en tête de file'
*)
let get_sommet f =
    match f.premier with
    | FVide  -> failwith "La file est vide, rien a faire (3)\n"
    | Elem x -> x.info ;; 


(* construit une liste à partir d'une _file *)
let to_liste file =
    match file.premier with
    | FVide  -> []
    | Elem x -> ( let rec aux = function
                      | FVide -> []
                      | Elem y -> 
                        let elem = y.info  in 
                                    elem::(aux y.next)
                      in (aux file.premier) ) ;; 



let affiche file =
    match file.premier with
    | FVide  -> print_string "[]\n"
    | Elem x -> (   let _ = print_string "[" in
                    let rec aux = function
                      | FVide -> Printf.printf "\b]\n"
                      | Elem y -> 
                        let _ = Printf.printf "%d " y.info  in 
                                    (aux y.next)
                      in (aux file.premier) ) ;; 


(*
    Met a jour la file, les sommets fermes sont retires
*)
let rec maj tab graphe_l file elem a_voisin = 
    match elem with
    | FVide -> ()
    | Elem x -> let a = x.info in
        if (a_voisin a graphe_l tab)
        then (maj tab graphe_l file x.next a_voisin)
        else 
            begin
                tab.(a) <- false ;
                (retirer_elem a file elem) ;
                (maj tab graphe_l file x.next a_voisin)
            end ;;
