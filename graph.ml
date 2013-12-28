
type 'a arc = 'a * 'a ;;
type 'a graph = ('a list) array ;;
type 'a graph_valued = (('a * int) list) array ;;

(* ========================================================================= *)
(*                           INITIALIZATION FUNCTION                         *)
(* ========================================================================= *)

(*
 * Initialize the array with true, the nodes are not yet visited.
 *)

let init_tab_VNV (taille : int) = (Array.make taille true) 

let init_lst (taille_max : int) (elem_max : int) (graph_pred : int graph) (s : int) 
: int list = 
  let lst = ref [] in 
  let taille = Random.int (taille_max / 2) in 
  for i = 0 to taille do 
    let aux = Random.int (elem_max - 1) + 1 in (* no arc on 0 -> for topological list *)
      if (aux = s) || (List.exists (fun a -> aux = a) (!lst))
      then () 
      else ( begin
               lst := aux::(!lst) ;
               graph_pred.(aux) <- s::(graph_pred.(aux)) ;
             end )
  done ;
  !lst ;;

(*
 * Return two graph, the graph can have circuit
 * Warning : they can't be a node with an arc on inself.
 *)

let init_graph (taille : int) : (int graph * int graph) =
  let graph_pred = Array.make taille [] in 
  let graph_succ = Array.make taille [] in 
  for i = 0 to (taille - 1) do 
    graph_succ.(i) <- init_lst taille taille graph_pred i
  done ;
  (graph_pred, graph_succ) ;;

(*
 * Return a valued graph
 *)

let init_graph_v (graph_succ : int graph) : (int graph_valued) =
  let taille = Array.length graph_succ in
  let graph_succ_v = Array.make taille [] in 
  for i = 0 to (taille - 1) do 

(* FIXME (1) :
 * The random is only for positive value ... go to FIXME (2)
 *)

    graph_succ_v.(i) <- List.map (fun x -> (x, (Random.int taille))) graph_succ.(i) 
  done ;
  graph_succ_v ;;

let init_lst_tree (taille_max : int) (elem_max : int) (graph_pred : int graph) (s : int) 
: int list = 
  let lst = ref [] in 
  let taille = Random.int (taille_max / 2) in 
  for i = 0 to taille do 
    let aux = Random.int (elem_max - 1) + 1 in (* no arc on 0 -> for topological list *)
      if (aux = s) || (List.exists (fun a -> aux = a) (!lst)) || (aux < s)
      then () 
      else ( begin
               lst := aux::(!lst) ;
               graph_pred.(aux) <- s::(graph_pred.(aux)) ;
             end )
  done ;
  !lst ;;

let init_tree (taille : int) : (int graph * int graph) =
  let graph_pred = Array.make taille [] in 
  let graph_succ = Array.make taille [] in 
  for i = 0 to (taille - 1) do 
    graph_succ.(i) <- init_lst_tree taille taille graph_pred i
  done ;
  (graph_pred, graph_succ) ;;

let generer_graphes (n : int) : ('a graph * 'a graph * 'a graph_valued) =
  let _ = print_string "Voulez vous générer un graphe :\n(0) : avec circuit\n(1) : sans circuit ?\n" in 
  let tree = read_int () in
  let (graph_pred, graph_succ) = if tree = 1
          then init_tree n
          else init_graph n in
  let graph_v = init_graph_v graph_succ in
  let _ = Printf.printf "\n\nTableau des listes d'adjacences : (sommets successeurs * coût) list : " in
  let _ = IO.print_array graph_v (IO.print_list (IO.print_pair print_int)) in
    (graph_pred, graph_succ, graph_v) ;;

(* ========================================================================= *)
(*                          FONCTIONS DE PARCOURS                            *)
(* ========================================================================= *)

let rec breadth_first_search (graph_succ : 'a graph) (file_nodes : int DeQueue.struct_file) 
(file_parcours : int DeQueue.struct_file) tab_VNV : int DeQueue.struct_file =
  if DeQueue.get_first file_nodes = DeQueue.FNull 
  then ( match (Tools.choose_node_nv tab_VNV graph_succ 0) with
         | -1     -> file_parcours
         | node ->  let _ = tab_VNV.(node) <- false in 
                      let f = (DeQueue.init ()) in 
                      let _ = (DeQueue.add node f) in
                      let _ = (DeQueue.add node file_parcours) in
                        (breadth_first_search graph_succ f file_parcours tab_VNV) )
  else ( let premier_node_file = (DeQueue.pop file_nodes) in
         let _ = ( try
                     let x = (Tools.choose_neighbor_nv premier_node_file graph_succ tab_VNV) in
                     let _ = tab_VNV.(x) <- false in
                     let _ = (DeQueue.add x file_parcours) in
                       (DeQueue.add x file_nodes)
                   with Tools.Close_Node -> begin 
                                          tab_VNV.(DeQueue.pop file_nodes) <- false ;
                                          (DeQueue.delete_head file_nodes) ; 
                                        end ) in
                   (breadth_first_search graph_succ file_nodes file_parcours tab_VNV) ) ;; 

(*

(* 
 * Update the stack, the closed node are deleted.
 *)

let rec maj_pile tab_VNV graph_succ = function
  | [] -> []
  | a::q -> if (a_voisin_nv a graph_succ tab_VNV) 
            then a::(maj_pile tab_VNV graph_succ q)
            else begin 
                   tab_VNV.(a) <- false ; 
                   (maj_pile tab_VNV graph_succ q) 
                 end ;;

let rec depth_first_search graph_succ pile_nodes liste_parcours tab_VNV =
  if pile_nodes = []
  then ( match (choisir_node_NV tab_VNV graph_succ 0) with
         | -1     -> liste_parcours
         | node -> let _ = (insere_file node liste_parcours) in 
                          (depth_first_search graph_succ [node] liste_parcours tab_VNV) )
  else
    let premier_node_pile = (get_node_pile pile_nodes) in 
    let x = (choisir_voisin_nonvisite premier_node_pile graph_succ tab_VNV) in
      if x = -1 then liste_parcours
      else
        begin
          tab_VNV.(x) <- false ;
          tab_VNV.(premier_node_pile) <- false ;
          let _ = (insere_file x liste_parcours) in
          let npile_nodes = (maj_pile tab_VNV graph_succ (insere_pile x pile_nodes)) in
            (depth_first_search graph_succ npile_nodes liste_parcours tab_VNV) ;
        end ;; 
*)

(* 
 * Depth_first_search with the recursion stack. 
 *)

let rec depth_rec (liste : 'a list ref) (node : 'a) (graph_succ : 'a graph) tab_bool =
  begin
    tab_bool.(node) <- true ;
    liste := node::(!liste) ;
    let succ_node = ref graph_succ.(node) in
    while (!succ_node <> []) do
      let x = List.hd !succ_node in
        if (not tab_bool.(x))
        then depth_rec liste x graph_succ tab_bool 
        else () ;
        succ_node := List.tl !succ_node
    done 
  end ;;

let depth_first_search (liste : ('a list) ref) (graph_succ : 'a graph) =
  let v = Array.make (Array.length graph_succ) false in
  for node = 0 to (Array.length graph_succ) - 1 do
    if (not v.(node))
    then depth_rec liste node graph_succ v
  done ;;

(* ========================================================================= *)
(*                   PLUS COURTS CHEMINS À ORIGINE FIXE                      *)
(* ========================================================================= *)

let bellman_ford (graph_succ : 'a graph_valued) (root : 'a) : ('a arc) list =
  let n = Array.length graph_succ in
  let d = Array.make n (100, None) in (* +∞ *)
  let _ = d.(root) <- (0, None) in 
  for k = 1 to n do
    let lst_arc = ref (Tools.get_lst_arc graph_succ 0) in
    while !lst_arc <> [] do
      let (x, y, c) = (List.hd (!lst_arc)) in
      let (dy, _) = d.(y) in
      let (dx, _) = d.(x) in

(* FIXME (2) : 
 * Quand dy et dx sont à +∞ et c(x, y) est negatif -> c'est un arc rouge 
 * alors que ces sommets ne sont pas forcement accessibles pas le sommet root 
 * Solutions : calcule des composantes fortement connexes, ...
 *)
   
      let _ = if dy > dx + c 
              then d.(y) <- (dx + c, (Some x)) in
      lst_arc := (List.tl (!lst_arc)) ;
    done ;
  done ;
  let lst_arc = ref (Tools.get_lst_arc graph_succ 0) in
  while !lst_arc <> [] do
    let (x, y, c) = (List.hd (!lst_arc)) in
    let (dy, _) = d.(y) in
    let (dx, _) = d.(x) in
    let _ = (if dy > dx + c 
            then failwith "Il y a une boucle absorbante") in
    lst_arc := (List.tl (!lst_arc)) ;
  done ;
  let red_arc_lst = ref [] in 
  for i = 0 to (n - 1) do 
    match d.(i) with
    | (c, Some x) when c < 50 -> red_arc_lst := (x, i)::(!red_arc_lst)
    | _           -> ()
  done ;
  !red_arc_lst ;;

(*
 * They can't be circuit
 *)

let rec construct_list_topo (graph_pred : int graph) (graph_succ : int graph) 
(lst : int list) : int list =
  try
  let (s, graph_pred) = Tools.choose_no_pred graph_pred graph_succ in
    (construct_list_topo graph_pred graph_succ (s::lst)) 
  with Tools.Has_Pred -> lst 

(*
 * Return a topological list with s1 = s
 *)

let construct_list_topo_from (s : 'a) (graph_pred : int graph) (graph_succ : int graph) 
(lst : int list) : int list =
   if graph_pred.(s) <> [] 
   then raise Tools.Has_Pred
   else ( let new_graph_pred = Array.copy graph_pred in
          let _ = Tools.delete_node_from_graph (graph_succ.(s)) s new_graph_pred in 
            construct_list_topo new_graph_pred graph_succ [s] ) ;;
 
(*
 * Warning : the graph can't have circuit.
 *)

let bellman (lst_topo : 'a list) (graph_succ_v : 'a graph_valued) (graph_pred : 'a graph) (graph_succ : 'a graph) (root : 'a) : ('a arc) list =
  let n = Array.length graph_succ in
         let d = Array.make n (100, None) in (* +∞ *)
         let _ = d.(root) <- (0, None) in
         for x = 0 to n - 1 do
           let lst_succ = (Tools.find_succ_topo graph_succ lst_topo x) in
           let lst_succ_cost = ref (Tools.assoc_cost_lst lst_succ graph_succ_v x) in
           while !lst_succ_cost <> [] do
             let (y, c) = (List.hd (!lst_succ_cost)) in
             let (dy, _) = d.(y) in 
             let (dx, _) = d.(x) in
             let _ = if dy > dx + c 
                     then d.(y) <- ((dx + c), (Some x)) in
               lst_succ_cost := (List.tl (!lst_succ_cost)) ;
          done ;
         done ;
         let red_arc_lst = ref [] in 
         for i = 0 to (n - 1) do 
           match d.(i) with
           | (_, Some x) -> red_arc_lst := (x, i)::(!red_arc_lst)
           | _           -> ()
         done ;
         !red_arc_lst  ;;
(*
 * dx have to be positive for all x
 *)

let dijkstra (graph_succ_v : 'a graph_valued) (root : 'a) : ('a arc) list =
  let n = Array.length graph_succ_v in
  let d = Array.make n (100, false, None) in (* +∞ *)
  let _ = d.(root) <- (0, true, None) in 
  let i = ref 0 in 
  while !i < n do 
    try
      let x = Tools.choose_open_dmin d in 
      let lst_succ = ref graph_succ_v.(x) in 
      while !lst_succ <> [] do 
        let (y, c) = (List.hd (!lst_succ)) in 
        let (dy, _, _) = d.(y) in 
        let (dx, _, _) = d.(x) in 
        let _ = if dy > dx + c 
                then d.(y) <- ((dx + c), true, (Some x)) in 
        lst_succ := (List.tl (!lst_succ)) ;
      done ;
      let _ = i := !i + 1 in 
      let (dx, _, fx) = d.(x) in
        d.(x) <- (dx, false, fx) ;
    with Tools.Close_Node -> i := n
  done ;
  let red_arc_lst = ref [] in 
    for y = 0 to (n - 1) do 
      match d.(y) with
      | (_, _,Some x) -> red_arc_lst := (x, y)::(!red_arc_lst)
      | _             -> ()
    done ;
    !red_arc_lst ;;
