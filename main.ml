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

  if (Array.length Sys.argv) <> 2 
  then failwith "Pas assez d'arguments : Vous devez entrer le nombre de noeuds dans le graphe" ;

  let taille = (int_of_string Sys.argv.(1)) in 
  let (graph_pred, graph_succ) = Graph.init_graph taille in
  (*Printf.printf "\nListe d'adjacence des sommets successeurs : " ;
  let _ = IO.print_array graph_succ (IO.print_list print_int) in*)
  let tab_VNV = Graph.init_tab_VNV taille in          
  let file_nodes = (DeQueue.init ()) in 
  (*let pile_nodes = (init_pile ()) in *)       
  begin 
    Printf.printf "\nUn parcours en largeur    : " ;
    let file_parcours_largeur = 
             (Graph.breadth_first_search graph_succ file_nodes (DeQueue.init ()) tab_VNV) in 
      DeQueue.print file_parcours_largeur ;

    Printf.printf "Un parcours en profondeur : " ;
    let liste_parcours = ref [] in
      Graph.depth_first_search liste_parcours graph_succ ;
    IO.print_list print_int (List.rev !liste_parcours) ;

    print_string "\n" ;

    Printf.printf "Une liste topologique des nodes : " ;
    let list_topo = Tools.construct_list_topo graph_pred graph_succ [] in 
      if (List.length list_topo) = (Array.length graph_pred) 
      then IO.print_list print_int (List.rev list_topo) 
      else Printf.printf "Il y a au moins un circuit dans le graphe\n\n" ;

    (* calcule des composantes fortement connexes *)

    let graph_v = Graph.init_graph_v graph_succ in 
      Printf.printf "\nListe d'adjacence des sommets successeurs et leur coût : " ;
    let _ = IO.print_array graph_v (IO.print_list (IO.print_pair print_int)) in
      print_newline () ;
    let _ = Printf.printf "\nArborescence :\n" in
    let lst_red_arc = Graph.bellman_ford graph_v 0 in
      IO.print_list (IO.print_pair print_int) lst_red_arc ;

    print_newline () ;  
    let oc = open_out "graph.dot" in
      output_string oc "digraph G {" ;
      output_string oc "\n" ;
      IO.output_graph oc ~graph_v:graph_v lst_red_arc IO.output_int ;

(* FIXME :
 * Sys.command "dot -Tps -o graph.ps graph.dot"
 * Sys.command "open graph.ps"
 * just execute the open command ...
 *)

    close_out oc ;
  end ;;