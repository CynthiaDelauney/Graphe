(* ========================================================================= *)
(*                                                                           *)
(* The graph is represented like a list array                                *)
(* The graph is oriented                                                     *)
(* The graph node are of type 'a                                             *)
(* The node can be compared with "="                                         *)
(* There is n node and m arc in the graph                                    *)
(* There is two sort of graph : valued (with positiv value for the moment)   *)
(* or note                                                                   *)
(*                                                                           *)
(* ========================================================================= *)


let () = 
  let _ = Random.self_init () in

  if Array.length Sys.argv <= 2 then failwith "Vous devez entrer 2 arguments"
    else (
    match Sys.argv.(1), (int_of_string Sys.argv.(2)) with
    | "graphe" , taille -> let (graph_pred, graph_succ) = Graph.init_tree taille in
                           let tab_VNV = Graph.init_tab_VNV taille in          
                           let file_nodes = (DeQueue.init ()) in 

                           let _ = Printf.printf "\nUn parcours en largeur    : " in
                           let file_parcours_largeur = 
                                   (Graph.breadth_first_search graph_succ file_nodes (DeQueue.init ()) tab_VNV) in 
                           let _ = DeQueue.print file_parcours_largeur in

                           let _ = Printf.printf "Un parcours en profondeur : " in
                           let liste_parcours = ref [] in
                           let _ = Graph.depth_first_search liste_parcours graph_succ in
                           let _ = IO.print_list print_int (List.rev !liste_parcours) in

                           (* Valued graph *)
                           let graph_v = Graph.init_graph_v graph_succ in 
                           let _ = Printf.printf "\n\nListe d'adjacence des sommets successeurs et leur coût : " in
                           let _ = IO.print_array graph_v (IO.print_list (IO.print_pair print_int)) in

                           let _ = Printf.printf "\nArborescence (avec bellman_ford) :\n" in
                           let lst_red_arc_bf = Graph.bellman_ford graph_v 0 in
                           let _ = IO.print_list (IO.print_pair print_int) lst_red_arc_bf in
                           let _ = IO.draw_graph graph_v lst_red_arc_bf in

                           let _ = Printf.printf "\n\nUne liste topologique des sommets : " in
                           let list_topo = (List.rev (Graph.construct_list_topo graph_pred graph_succ [])) in 
                             if (List.length list_topo) = (Array.length graph_pred) 
                             then ( let _ = IO.print_list print_int list_topo in
                                    let _ = Printf.printf "\n\nArborescence avec bellman :\n" in
                                    let lst_red_arc_f = Graph.bellman list_topo graph_v graph_pred graph_succ 0 in
                                      IO.print_list (IO.print_pair print_int) lst_red_arc_f )
                             else Printf.printf "\nIl y a au moins un circuit dans le graphe\n\n" ;

    | "courbe" , n -> let oc = open_out "donnees.dat" in 
                      for j = 2 to n do
                        let _ = Printf.printf "#%d\n" j in
                        let _ = flush stdout in
                        let moy1 = ref 0. and moy2 = ref 0. in
                        for i = 1 to 50 do
                          let taille = j in 
                          let (graph_pred, graph_succ) = Graph.init_tree taille in
                          let graph_v = Graph.init_graph_v graph_succ in
                          let n1 = Sys.time () in
                          let _ = Graph.bellman_ford graph_v 0 in
                          let n2 = Sys.time () in
                          let _ = moy1 := !moy1 +. (n2 -. n1) in
                          let list_topo = (List.rev (Graph.construct_list_topo graph_pred graph_succ [])) in 
                             if (List.length list_topo) = (Array.length graph_pred) 
                             then ( let n3 = Sys.time () in 
                                    let _ = Graph.bellman list_topo graph_v graph_pred graph_succ 0 in 
                                    let n4 = Sys.time () in 
                                      moy2 := !moy2 +. (n4 -. n3) )
                        done ;
                        let _ = moy1 := !moy1 /. 50. in 
                        let _ = moy2 := !moy2 /. 50. in 
                          IO.output_ligne oc j !moy1 !moy2 ;
                      done ;
                      close_out oc ;
                      if Sys.command "gnuplot genere_cpu_courbe.gnu" = 0 then ()
                      else print_string "Erreur lors de la génération de la courbe.\n" ;
   
    | _ -> failwith "hkjhkj" ) ;
    print_newline () ;

