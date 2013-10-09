

(*
    val tab_VNV : bool array 
    le true signifie que le sommet est non visite, le false qu'il est visite,
    par defaut tous les sommets sont visites.
*)
let init_tab_VNV () = (Array.make 13 true) ;;

(* val visite : int -> 'a array -> 'a = <fun> *)
let visite = function sommet -> function tab_VNV -> tab_VNV.(sommet) ;;

(* 
    val a_voisin : int -> int list array -> bool array -> bool = <fun> 
    retourne vrai si le sommet x a au moins un voisin non visite
*)
let a_voisin = function x -> function graphe_l -> function tab_VNV ->
    let liste_sommets_adj = graphe_l.(x) in
        (*
            'aux' parcourt la liste, et dit si cette liste contient au moins un sommet non visite 
        *)
        let rec aux = function liste -> function tab_VNV ->
            if liste = []
            then false
            else 
                ( if (visite (List.hd liste) tab_VNV)
                  then true
                  else (aux (List.tl liste) tab_VNV) )
        in
        (aux liste_sommets_adj tab_VNV) ;;

(* val init_file : unit -> 'a list = <fun> *)
let init_file () = [] ;;

(* val affiche_list : int list -> unit = <fun> *)
let affiche_list = function liste ->
    let _ = Printf.printf "[" in
        let rec aux = function
            | []    -> Printf.printf "[]\n"
            | a::[] -> Printf.printf "%d]\n" a
            | a::q  -> let _ = (Printf.printf "%d;" a) in (aux q) 
        in 
        (aux liste) ;;

(* 
    val affiche_tab : bool array -> unit = <fun>
*)
let affiche_tab = function tab ->
    match (Array.length tab) with 
    | 0 -> Printf.printf "[||]\n" 
    | 1 -> Printf.printf "[|%B|]\n" tab.(0)
    | t -> let _ = Printf.printf "[|%B;" tab.(0) in
                for i = 1 to t - 2 do 
                    Printf.printf "%B;" tab.(i) 
                done ;
                Printf.printf "%B|]\n" tab.(t - 1) ;;

(* 
    val insere_file : 'a -> 'a list -> 'a list = <fun> 
    insere elem a la fin de la liste
*)
let rec insere_file = function elem -> function 
    | []    -> [elem]    
    | a::q  -> a::(insere_file elem q) ;;

(* 
    val enleve_tete : 'a list -> 'a list = <fun> 
    enleve le premier element de la liste
*)
let enleve_tete = function
    | []    -> failwith "La file est vide, rien a faire\n"
    | a::q  -> q ;;

(* 
    val maj_file : int list -> bool array -> int list array -> int list = <fun> 
    Met a jour la file d'attente, les sommets fermes sont retires
*)
let rec maj_file = function tab_VNV -> function graphe_l -> function
    | [] -> []
    | a::q -> if (a_voisin a graphe_l tab_VNV) 
              then a::(maj_file tab_VNV graphe_l q)
              else begin tab_VNV.(a) <- false ; (maj_file tab_VNV graphe_l q) end ;;

(*
    val choisir_voisin_nonvisite : int -> int list array -> bool array -> int = <fun>
    hyp : sommet a un voisin non visite
    renvoie le premier sommet non visite de la liste
*)
let choisir_voisin_nonvisite = function sommet -> function graphe_l -> function tab_VNV ->
    let liste = graphe_l.(sommet) in
        let rec aux = function liste -> function tab_VNV ->
            if liste = [] 
            then failwith "Liste sans sommets visites\n"
            else (match (visite (List.hd liste) tab_VNV) with
                  | true -> (List.hd liste)
                  | _ -> (aux (List.tl liste) tab_VNV))
        in
        (aux liste tab_VNV) ;; 

let rec choisir_sommet_NV = function tab_VNV -> function graphe_l -> function
    | t     when t = (Array.length graphe_l) -> - 1
    | i     -> if (visite i tab_VNV) then i else (choisir_sommet_NV tab_VNV graphe_l (i + 1)) ;;


let rec parcours_largeur = function graphe_l -> function file_sommets -> function liste_parcours -> function tab_VNV ->
    if file_sommets = [] 
    then ( match (choisir_sommet_NV tab_VNV graphe_l 0) with
           | -1     -> liste_parcours
           | sommet -> (parcours_largeur graphe_l [sommet] (insere_file sommet liste_parcours) tab_VNV) )
    else
        let premier_sommet_file = (List.hd file_sommets) in
            let x = (choisir_voisin_nonvisite premier_sommet_file graphe_l tab_VNV) in
                begin
                tab_VNV.(x) <- false ;
                let nliste_parcours = (insere_file x liste_parcours) in
                    let nfile_sommets = (maj_file tab_VNV graphe_l (insere_file x file_sommets)) in
                        (parcours_largeur graphe_l nfile_sommets nliste_parcours tab_VNV) ;
                end ;; 


let main =

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

        let tab_VNV = init_tab_VNV () in            (* initialise le tableau de bool à true *)
        let liste_parcours = (init_file ()) in      (* liste vide *)
        let file_sommets = (init_file ()) in        (* [] *)
            (affiche_list (parcours_largeur graphe_l file_sommets liste_parcours tab_VNV)) ;;

