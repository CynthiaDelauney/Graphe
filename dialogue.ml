exception Break

let dialogue () =
  
  print_string "Combien de noeuds ?\n" ;
  let n = read_int () in

  print_string "Voulez vous un graphe\n(0) : orienté\n(1) : non orienté ?\n" ;
  let oriented = read_int () in

  let graph_pred = ref [||] in 
  let graph_succ = ref [||] in 
  let graph_v    = ref [||] in

  let (g1, g2, g3) = Graph.generer_graphes (if oriented = 0 then true else false) n

  in

  graph_pred := g1 ;
  graph_succ := g2 ;
  graph_v := g3 ;

  let continue = ref true in

  while !continue do
    let _ = print_string "\nSommet de départ s = ? ou (-1) pour quitter\n" in
    let s = read_int () in 

    match s with
    | -1 -> continue := false
    | s when s >= n -> Printf.printf "n = %d, s doit être inférieur à s" n
    | _ -> (*let file_nodes = (DeQueue.init ()) in 
           let tab_VNV = Graph.init_tab_VNV n in 
           let _ = Printf.printf "\nUn parcours en largeur    : " in
           let file_parcours_largeur = 
             (Graph.breadth_first_search !graph_succ file_nodes (DeQueue.init ()) tab_VNV) in 
           let _ = DeQueue.print file_parcours_largeur in

           let _ = Printf.printf "Un parcours en profondeur : " in
           let liste_parcours = ref [] in
           let _ = Graph.depth_first_search liste_parcours !graph_succ in
           let _ = IO.print_list print_int (List.rev !liste_parcours) in*)

           Printf.printf "\n\nArborescence (avec bellman_ford) des plus court chemin, d'origine s  :\n" ;
           let lst_red_arc_bf = Graph.bellman_ford !graph_v s in
           IO.print_list (IO.print_pair print_int print_int) lst_red_arc_bf ;
(*
           let _ = Printf.printf "\nUne liste topologique des sommets avec s = %d : " s in
           let _ =
           try
             let list_topo = (List.rev (Graph.construct_list_topo_from s !graph_pred !graph_succ [])) in 
             if (List.length list_topo) = (Array.length !graph_pred) 
             then (IO.print_list print_int list_topo ; print_newline ())
             else ( let _ = Printf.printf "\nImpossible, il y a au moins un circuit dans le graphe\n\n" in 
                    let _ = print_string "Generer un nouveau graphe ? (1 pour en générer un, sinon 0)\n" in 
                    let generer = read_int () in 
                    if generer = 1 
                    then begin
                           let (g1, g2, g3) = Graph.generer_graphes (if oriented = 0 then true else false) n in
                           let _ = graph_pred := g1 in
                           let _ = graph_succ := g2 in graph_v := g3
                         end 
                    else raise Break ); 
           with Tools.Has_Pred -> Printf.printf "\nImpossible, le sommet s a des predecesseurs\n\n"
              | Break -> (continue := false) in
*)
           print_string "\nArborescence (avec Prim) : \n" ;
           let lst_yellow_arc = Graph.prim !graph_v s in
           IO.print_list (IO.print_pair print_int print_int) lst_yellow_arc ;

           print_string "\nVoulez vous dessiner le graphe avec gnuplot ? (1 pour oui, sison 0)\n" ;
           let dessiner = read_int () in 
           if dessiner = 1 
           then begin IO.draw_arbo "red" lst_red_arc_bf ;
                      if Sys.command "dot -Tps -o arbo.ps arbo.dot" = 0 
                      then (if Sys.command "open arbo.ps" = 1 then print_string "error open graph.ps")
                      else print_string "error gnuplot"
                end ;
           print_newline ()   

  done ;;
