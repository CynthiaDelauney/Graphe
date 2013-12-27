
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
 * Updated the stack, the closed node are deleted.
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
 * Quand dy et dx sont à +∞ et c(x, y) est nagatif -> c'est un arc rouge 
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
  let (s, graph_pred) = Tools.choose_less_pred graph_pred graph_succ in
    (construct_list_topo graph_pred graph_succ (s::lst)) 
  with Tools.Empty_Graph -> lst 

(*
 * Warning : the graph can't have circuit.
 *)

let bellman (lst_topo : 'a list) (graph_succ_v : 'a graph_valued) (graph_pred : 'a graph) (graph_succ : 'a graph) (root : 'a) : ('a arc) list =
  if (lst_topo = []) || (List.hd lst_topo) <> 0 
  then failwith "s has predecessors"
  else ( let n = Array.length graph_succ in
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
         !red_arc_lst ) ;;
