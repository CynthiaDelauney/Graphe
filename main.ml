(* ========================================================================= *)
(*                                                                           *)
(* The graph is represented like a list array                                *)
(* The graph is oriented                                                     *)
(* The graph node are of type 'a                                             *)
(* The node can be compared with "="                                         *)
(* There is n node and m arc in the graph                                    *)
(* There is two sort of graph : valued (with positiv value for the moment)   *)
(* or not                                                                    *)
(*                                                                           *)
(* ========================================================================= *)

let () = 
  let _ = Random.self_init () in

  let oriented = false in

  ( match Sys.argv.(1) with
    | "-graph"  -> Dialogue.dialogue ()
 
    | "-courbe" -> let n = (int_of_string Sys.argv.(2)) in
                   let oc = open_out "donnees.dat" in 
                   for j = 2 to n do
                     Printf.printf "#%d\n" j ;
                     flush stdout ;
                     let moy_bellman_ford = ref 0. and moy_bellman = ref 0. and moy_dijkstra = ref 0. in
                     for i = 1 to 50 do
                       let taille = j in 
                       let (graph_pred, graph_succ) = Graph.init_tree oriented taille in
                       let graph_v = Graph.init_graph_v oriented graph_succ in

                       let n1 = Sys.time () in   
                       let _ = Graph.bellman_ford graph_v 0 in 
                       let n2 = Sys.time () in 
                       moy_bellman_ford := !moy_bellman_ford +. (n2 -. n1) ;
                       let n5 = Sys.time () in
                       let _ = Graph.dijkstra graph_v 0 in
                       let n6 = Sys.time () in
                       let _ = moy_dijkstra := !moy_dijkstra +. (n6 -. n5) in

                       let list_topo = (List.rev (Graph.construct_list_topo graph_pred graph_succ [])) in 
                         if (List.length list_topo) = (Array.length graph_pred) 
                         then ( let n3 = Sys.time () in 
                                let _ = Graph.bellman list_topo graph_v graph_pred graph_succ 0 in 
                                let n4 = Sys.time () in moy_bellman := !moy_bellman +. (n4 -. n3) )
                     done ;
                     moy_bellman_ford := !moy_bellman_ford /. 50. ;
                     moy_dijkstra := !moy_dijkstra /. 50. ;
                     moy_bellman := !moy_bellman /. 50. ;
                     IO.output_ligne oc j !moy_bellman_ford !moy_dijkstra !moy_bellman ;
                   done ;
                   close_out oc ;
                   if Sys.command "gnuplot genere_cpu_courbe.gnu" = 0 
                   then ()
                   else print_string "Erreur lors de la génération de la courbe.\n" 
    | _ -> failwith "-graph or -courbe with param" ) ;
    print_newline () ;;

