

(* =================================================================== *)

(* STRUCTURE DE FILE *)

type 'a elem = { mutable prev : 'a _elem ;
                 mutable next : 'a _elem ;
                 info : 'a               }
and 'a _elem =
    | FVide
    | Elem of 'a elem ;;

type 'a _file = { mutable premier : 'a _elem ;
                  mutable dernier : 'a _elem  } ;;

let init_file () = {premier = FVide ; dernier = FVide} ;;

(* 
    val insere_file : 'a -> 'a file -> unit = <fun> 
    insere v dans la file
*)
let insere_file v f =
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
let enleve_tete_file f =
    match f.premier with
    | FVide -> failwith "La file est vide, rien a faire\n"
    | Elem x -> let npremier = x.next in
                match npremier with
                | FVide -> f.premier <- FVide ; f.dernier <- FVide
                | Elem y -> let _ = y.prev <- FVide in
                            f.premier <- npremier ;; 

(* 
    val retirer_elem_file : 'a -> 'a _file -> unit = <fun> 
    retire l'element elem de la file
*)
let rec retirer_elem_file a file elem =
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
let get_sommet_file f =
    match f.premier with
    | FVide  -> failwith "La file est vide, rien a faire (3)\n"
    | Elem x -> x.info ;; 

(* =================================================================== *)

(* STRUCTURE DE PILE *)

let init_pile () = [] 

(* 
    val insere_pile : 'a -> 'a list -> 'a list = <fun> 
    insere elem au debut de la liste
*)
let insere_pile elem pile = elem::pile

(*
    val enleve_tete_liste : 'a list -> 'a list = <fun>
    enleve le premier element de la liste
*)
let enleve_tete_liste = function
    | []   -> failwith "La pile est vide, rien à faire (4)\n"
    | a::q -> q

(*  
    val get_sommet_pile : 'a list -> 'a = <fun>
    retourne le sommet de la pile, premier elem de la liste
*)
let get_sommet_pile = function
    | []   -> failwith "La pile est vide, rien à faire (5)\n"
    | a::q -> a

(* =================================================================== *)

(* FONCTIONS D'AFFICHAGE *)

(* 
    val affiche_tab : bool array -> unit = <fun>
*)
let affiche_tab tab =
    match (Array.length tab) with 
    | 0 -> Printf.printf "[||]\n" 
    | 1 -> Printf.printf "[|%B|]\n" tab.(0)
    | t -> let _ = Printf.printf "[|%B;" tab.(0) in
                for i = 1 to t - 2 do 
                    Printf.printf "%B;" tab.(i) 
                done ;
                Printf.printf "%B|]\n" tab.(t - 1) ;;

(* val affiche_list : int list -> unit = <fun> *)
let affiche_list liste =
    let _ = Printf.printf "[" in
        let rec aux = function
            | []    -> Printf.printf "[]\n"
            | a::[] -> Printf.printf "%d]\n" a
            | a::q  -> let _ = (Printf.printf "%d;" a) in (aux q) 
        in 
            (aux liste) ;;

let affiche_file file =
    match file.premier with
    | FVide  -> print_string "[]\n"
    | Elem x -> (   let _ = print_string "[" in
                    let rec aux = function
                      | FVide -> Printf.printf "\b]\n"
                      | Elem y -> 
                        let _ = Printf.printf "%d " y.info  in 
                                    (aux y.next)
                      in (aux file.premier) ) ;; 

(* =================================================================== *)

(* UTILITAIRE *)

(*
    val tab_VNV : bool array 
    le true signifie que le sommet est non visité, le false qu'il est visité,
    par defaut tous les sommets sont non visités.
*)
let init_tab_VNV () = (Array.make 13 true) 


(* val non_visite : int -> 'a array -> 'a = <fun> *)
let non_visite sommet tab_VNV = tab_VNV.(sommet) ;;

(* 
    val a_voisin : int -> int list array -> bool array -> bool = <fun> 
    retourne vrai si le sommet x a au moins un voisin non visite
*)
let a_voisin x graphe_l tab_VNV =
    let liste_sommets_adj = graphe_l.(x) in
        (* 'aux' parcourt la liste, et dit si cette liste contient au moins un sommet non visite *)
        let rec aux liste tab_VNV = 
            if liste = []
            then false
            else 
                ( if (non_visite (List.hd liste) tab_VNV)
                  then true
                  else (aux (List.tl liste) tab_VNV) )
        in
            (aux liste_sommets_adj tab_VNV) ;;

(*
    val choisir_voisin_nonvisite : int -> int list array -> bool array -> int = <fun>
    hyp : sommet a un voisin non visite
    renvoie le premier sommet non visite de la liste
*)
let choisir_voisin_nonvisite sommet graphe_l tab_VNV =
    let liste = graphe_l.(sommet) in
        let rec aux = function liste -> function tab_VNV ->
            if liste = [] 
            then -1
            else (match (non_visite (List.hd liste) tab_VNV) with
                  | true -> (List.hd liste)
                  | _    -> (aux (List.tl liste) tab_VNV))
        in
            (aux liste tab_VNV) ;; 

(*
    on parcourt tout les sommets, jusqu'a en trouver un qui soit non visité 
*)
let rec choisir_sommet_NV tab_VNV graphe_l sommet =
    if sommet = (Array.length graphe_l) then -1
    else (  if (non_visite sommet tab_VNV) 
            then sommet 
            else (choisir_sommet_NV tab_VNV graphe_l (sommet + 1)) ) ;;

(* 
    vam maj_pile : bool array -> int list array -> int list -> int list = <fun>
    Met a jour la pile, les sommets fermes sont retires
*)
let rec maj_pile tab_VNV graphe_l = function
    | [] -> []
    | a::q -> if (a_voisin a graphe_l tab_VNV) 
              then a::(maj_pile tab_VNV graphe_l q)
              else begin 
                   tab_VNV.(a) <- false ; 
                   (maj_pile tab_VNV graphe_l q) 
                   end ;;

(*
    val maj_file : bool array -> int list array -> int _file -> 'a list = <fun> 
    Met a jour la file, les sommets fermes sont retires
*)
let rec maj_file tab_VNV graphe_l file elem = 
    match elem with
    | FVide -> ()
    | Elem x -> let a = x.info in
        if (a_voisin a graphe_l tab_VNV)
        then (maj_file tab_VNV graphe_l file x.next)
        else 
            begin
                tab_VNV.(a) <- false ;
                (retirer_elem_file a file elem) ;
                (maj_file tab_VNV graphe_l file x.next)
            end ;;


(* =================================================================== *)

(* FONCTIONS DE PARCOURS *)

(* int list array -> int list -> int list -> bool array -> int list = <fun> *)
let rec parcours_largeur graphe_l file_sommets liste_parcours tab_VNV =
    if file_sommets.premier = FVide 
    then ( match (choisir_sommet_NV tab_VNV graphe_l 0) with
           | -1     -> liste_parcours
           | sommet ->  let _ = tab_VNV.(sommet) <- false in 
                        let f = (init_file ()) in 
                        let _ = (insere_file sommet f) in
                        let _ = (insere_file sommet liste_parcours) in
                            (parcours_largeur graphe_l f liste_parcours tab_VNV) )
    else
        let premier_sommet_file = (get_sommet_file file_sommets) in
            let x = (choisir_voisin_nonvisite premier_sommet_file graphe_l tab_VNV) in
                begin
                tab_VNV.(x) <- false ;
                let _ = (insere_file x liste_parcours) in
                let _ = (insere_file x file_sommets) in
                    let _ = (maj_file tab_VNV graphe_l file_sommets file_sommets.premier) in
                        (parcours_largeur graphe_l file_sommets liste_parcours tab_VNV) ;
                end ;; 

(* int list array -> int list -> int list -> bool array -> int list = <fun> *)
let rec parcours_profondeur graphe_l pile_sommets liste_parcours tab_VNV =
    if pile_sommets = []
    then ( match (choisir_sommet_NV tab_VNV graphe_l 0) with
           | -1     -> liste_parcours
           | sommet -> let _ = (insere_file sommet liste_parcours) in 
                            (parcours_profondeur graphe_l [sommet] liste_parcours tab_VNV) )
    else
        let premier_sommet_pile = (get_sommet_pile pile_sommets) in 
            let x = (choisir_voisin_nonvisite premier_sommet_pile graphe_l tab_VNV) in
                if x = -1 then liste_parcours
            else
                begin
                tab_VNV.(x) <- false ;
                tab_VNV.(premier_sommet_pile) <- false ;
                let _ = (insere_file x liste_parcours) in
                    let npile_sommets = (maj_pile tab_VNV graphe_l (insere_pile x pile_sommets)) in
                        (parcours_profondeur graphe_l npile_sommets liste_parcours tab_VNV) ;
                end ;; 

(* =================================================================== *)

let () =

    (*
        val graphe_l : int list array 
        c'est la liste d'adjacence du graphe
    *)
    let graphe_l = [|
            (* 0 *)  [1 ; 3]              ;
            (* 1 *)  [0 ; 2 ; 3]          ; 
            (* 2 *)  [1]                  ;
            (* 3 *)  [0 ; 1]              ;
            (* 4 *)  [6 ; 8 ; 9 ; 10 ; 5] ;
            (* 5 *)  [4 ; 7]              ;
            (* 6 *)  [4 ; 10]             ;
            (* 7 *)  [5]                  ;
            (* 8 *)  [4 ; 9]              ;
            (* 9 *)  [4 ; 8]              ;
            (* 10 *) [4 ; 6]              ;
            (* 11 *) [12]                 ;
            (* 12 *) [11]                 ;
                   |]
    in
        let tab_VNV = init_tab_VNV () in          
        let file_sommets = (init_file ()) in 
        let pile_sommets = (init_pile ()) in        
            begin
            Printf.printf "\nParcours en largeur : " ;
            (affiche_file (parcours_largeur graphe_l file_sommets (init_file ()) tab_VNV)) ;
            Printf.printf "Parcours en profondeur : " ;
            (affiche_file (parcours_profondeur graphe_l pile_sommets (init_file ()) (init_tab_VNV ()))) ;
            print_string "\n"
            end ;;

