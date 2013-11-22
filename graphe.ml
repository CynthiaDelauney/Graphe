
(* =================================================================== *)

(* FONCTIONS D'AFFICHAGE *)

let affiche_tab tab =
    match (Array.length tab) with 
    | 0 -> Printf.printf "[||]\n" 
    | 1 -> Printf.printf "[|%B|]\n" tab.(0)
    | t -> let _ = Printf.printf "[|%B;" tab.(0) in
                for i = 1 to t - 2 do 
                    Printf.printf "%B;" tab.(i) 
                done ;
                Printf.printf "%B|]\n" tab.(t - 1) ;;

let affiche_list liste =
    let _ = Printf.printf "[" in
        let rec aux = function
            | []    -> Printf.printf "[]\n"
            | a::[] -> Printf.printf "%d]\n" a
            | a::q  -> let _ = (Printf.printf "%d " a) in (aux q) 
        in 
            (aux liste) ;;

let output_int = function oc -> function x ->
    output_string oc (string_of_int x) ;;

let rec est_arc_rouge (a, b) = function
    | [] -> false
    | (ah, bh)::q -> if ((a = ah && b = bh) || (a = bh && b = ah))
                     then true 
                     else (est_arc_rouge (a, b) q) ;;

let rec out_put_sommet oc liste_arc_rouge sommet = function
    | [] -> () 
    | a::q ->   begin
                    output_int oc sommet ;
                    output_string oc " -- " ;
                    output_int oc a ;
                    if (est_arc_rouge (sommet, a) liste_arc_rouge)
                    then output_string oc "[color=red];\n" 
                    else output_string oc ";\n" ;
                    (out_put_sommet oc liste_arc_rouge sommet q)
                end ;;


let out_put_graphe oc graphe_l liste_arc_rouge =
    for i = 0 to (Array.length graphe_l) - 1 do 
        out_put_sommet oc liste_arc_rouge i graphe_l.(i) 
    done  ;
    output_string oc "}" ;;

(* =================================================================== *)

(* UTILITAIRES *)

(*
 * On initialise le tableau à true, les sommets sont non visités
 *)

let init_tab_VNV () = (Array.make 13 true) 

let non_visite sommet tab_VNV = tab_VNV.(sommet) ;;

(* 
 * a_voisin x retourne vrai si le sommet x a au moins un voisin non visité
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
 * choisir_voisin_nonvisite renvoie le premier sommet non visite de la liste
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
 * on parcourt tout les sommets, jusqu'a en trouver un qui soit non visité 
 *)

let rec choisir_sommet_NV tab_VNV graphe_l sommet =
    if sommet = (Array.length graphe_l) then -1
    else (  if (non_visite sommet tab_VNV) 
            then sommet 
            else (choisir_sommet_NV tab_VNV graphe_l (sommet + 1)) ) ;;

(* 
 * Met a jour la pile, les sommets fermes sont retires
 *)

let rec maj_pile tab_VNV graphe_l = function
    | [] -> []
    | a::q -> if (a_voisin a graphe_l tab_VNV) 
              then a::(maj_pile tab_VNV graphe_l q)
              else begin 
                   tab_VNV.(a) <- false ; 
                   (maj_pile tab_VNV graphe_l q) 
                   end ;;


(* =================================================================== *)

(* FONCTIONS DE PARCOURS *)

let rec parcours_largeur graphe_l file_sommets liste_parcours tab_VNV =
    if DeQueue.get_premier file_sommets = DeQueue.FVide 
    then ( match (choisir_sommet_NV tab_VNV graphe_l 0) with
           | -1     -> liste_parcours
           | sommet ->  let _ = tab_VNV.(sommet) <- false in 
                        let f = (DeQueue.init ()) in 
                        let _ = (DeQueue.insere sommet f) in
                        let _ = (DeQueue.insere sommet liste_parcours) in
                            (parcours_largeur graphe_l f liste_parcours tab_VNV) )
    else
        let premier_sommet_file = (DeQueue.get_sommet file_sommets) in
            let x = (choisir_voisin_nonvisite premier_sommet_file graphe_l tab_VNV) in
                begin
                tab_VNV.(x) <- false ;
                let _ = (DeQueue.insere x liste_parcours) in
                let _ = (DeQueue.insere x file_sommets) in
                    let _ = (DeQueue.maj tab_VNV graphe_l file_sommets (DeQueue.get_premier file_sommets) a_voisin) in
                        (parcours_largeur graphe_l file_sommets liste_parcours tab_VNV) ;
                end ;; 

(*
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
*)

(* Parcours en profondeur, en utilisant la pile de recursion *)

let rec descente_rec liste sommet graphe_l tab_bool =
    begin
        tab_bool.(sommet) <- true ;
        liste := sommet::(!liste) ;
        let succ_sommet = ref graphe_l.(sommet) in
        while (!succ_sommet <> []) do
            let x = List.hd !succ_sommet in
            if (not tab_bool.(x))
            then descente_rec liste x graphe_l tab_bool 
            else () ;
            succ_sommet := List.tl !succ_sommet
        done 
    end ;;

let parcours_profondeur liste graphe_l =
    let v = Array.make (Array.length graphe_l) false in
    for sommet = 0 to (Array.length graphe_l) - 1 do
        if (not v.(sommet))
        then descente_rec liste sommet graphe_l v
    done ;;

(* =================================================================== *)

let () =

    (*
     * Liste d'adjacence du graphe
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
        let file_sommets = (DeQueue.init ()) in 
        (*let pile_sommets = (init_pile ()) in *)       
            begin  

                Printf.printf "\nParcours en largeur :    " ;

                let file_parcours_largeur = (parcours_largeur graphe_l file_sommets (DeQueue.init ()) tab_VNV) in 
                let _ = DeQueue.affiche file_parcours_largeur in
                
                (* let oc = open_out "graphe_l.dot" in
                    output_string oc "graph G {" ;
                    output_string oc "\n" ;
                    out_put_graphe oc graphe_l [] ;
                *)
                let _ = Printf.printf "Parcours en profondeur : " in
                let liste_parcours = ref [] in
                let _ = parcours_profondeur liste_parcours graphe_l in
                let _ = affiche_list (List.rev !liste_parcours) in
                    print_string "\n" ;
                (*
                if Sys.command "dot -Tps -o graphe_l.ps graphe_l.dot" = 0 
                then if Sys.command "open graphe_l.ps" = 0 then () else 
                        print_string "Erreur lors de l'ouverture du fichier graphe_l.ps\n"
                else print_string "Erreur lors de la génération du graphe.\n" ;
                close_out oc ;
                *)
            end ;;

