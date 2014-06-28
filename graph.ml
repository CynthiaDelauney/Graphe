
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

let init_graph (oriented : bool) (taille : int) : (int graph * int graph) =
  let graph_pred = Array.make taille [] in 
  let graph_succ = Array.make taille [] in 
  for i = 0 to (taille - 1) do 
    graph_succ.(i) <- init_lst taille taille graph_pred i
  done ;
  (graph_pred, graph_succ) ;;

(*
 * Return a valued graph
 * FIXME (1) :
 * The random is only for positive value ... go to FIXME (2)
 *)

let init_graph_v (oriented : bool) (graph_succ : int graph) : (int graph_valued) =
  let taille = Array.length graph_succ in
  let graph_succ_v = Array.make taille [] in
  if oriented 
  then for i = 0 to (taille - 1) do
         graph_succ_v.(i) <- List.map (fun x -> (x, (Random.int taille))) graph_succ.(i) 
       done 
  else ( let tab_cost = Array.make taille [||] in 
         for i = 0 to taille - 1 do tab_cost.(i) <- Array.make taille (-1) done ; (* -1 for not initialized value *)
         for x = 0 to taille - 1 do 
           graph_succ_v.(x) <- List.map (fun y -> (y, (if (tab_cost.(x).(y) <> -1) 
                                                       then tab_cost.(x).(y)
                                                       else ( if (tab_cost.(y).(x) <> -1) 
                                                              then tab_cost.(y).(x)
                                                              else ( let aux = (Random.int taille) in 
                                                                     tab_cost.(x).(y) <- aux ;
                                                                     tab_cost.(y).(x) <- aux ;
                                                                       aux ) ) ))) graph_succ.(x)
         done ) ;
  graph_succ_v ;;

let init_lst_tree_oriented (taille_max : int) (elem_max : int) (graph_pred : int graph) (s : int) 
: int list = 
  let lst = ref [] in 
  let taille = Random.int (taille_max / 2) in 
  for i = 0 to taille do 
    let aux = Random.int (elem_max - 1) + 1 in (* no arc on 0 -> for topological list *)
      if (List.exists (fun a -> aux = a) !lst) || (aux <= s)
      then () 
      else ( begin
               lst := aux::(!lst) ;
               graph_pred.(aux) <- s::(graph_pred.(aux)) ;
             end )
  done ;
  !lst ;;

let init_lst_tree (tab_cc : int array) (taille_max : int) (elem_max : int) (graph_succ : int graph) (s :int)
: int list =
  let lst = ref (if graph_succ.(s) = [] 
                 then []  
                 else graph_succ.(s)) in
  let taille = Random.int (taille_max / 2) in 
  for i = 0  to taille do
    let aux = Random.int (elem_max - 1) in
    if (List.exists (fun a -> a = aux) !lst) || (aux <=s) || ((tab_cc.(aux) <> -1) && (tab_cc.(aux) = tab_cc.(s)))
    then () 
    else ( begin
             lst := aux::(!lst) ;
             if tab_cc.(s) = -1 
             then (tab_cc.(s) <- s ; tab_cc.(aux) <- tab_cc.(s))
             else ( let e = tab_cc.(aux) in 
                    tab_cc.(aux) <- tab_cc.(s) ; 
                    if e <> -1 
                    then
                      for x = 0 to taille_max - 1 do  
                        if tab_cc.(x) = e then tab_cc.(x) <- tab_cc.(s)
                      done ) ;
             graph_succ.(aux) <- s::(graph_succ.(aux)) ;
           end )
  done ;
  !lst ;; 

let init_tree (oriented : bool) (taille : int) : (int graph * int graph) =
  let graph_pred = Array.make taille [] in 
  let graph_succ = Array.make taille [] in 
  if oriented
  then for i = 0 to (taille -1) do 
         graph_succ.(i) <- init_lst_tree_oriented taille taille graph_pred i 
       done 
  else ( let tab_cc = Array.make taille (-1) in 
         tab_cc.(0) <- 0 ; 
         for i = 0 to (taille - 1) do 
           graph_succ.(i) <- init_lst_tree tab_cc taille taille graph_succ i
         done ; IO.print_array tab_cc print_int) ;
  (graph_pred, graph_succ) ;;

let generer_graphes (oriented : bool) (n : int) : ('a graph * 'a graph * 'a graph_valued) =
  print_string "Voulez vous générer un graphe :\n(0) : avec circuit/cycle\n(1) : sans circuit/cycle (arbre) ?\n" ; 
  let tree = read_int () in
  let (graph_pred, graph_succ) = if tree = 1
                                 then init_tree oriented n
                                 else init_graph oriented n in
  let graph_v = init_graph_v oriented graph_succ in
  Printf.printf "\n\nTableau des listes d'adjacences : (sommets successeurs * coût) list : " ;
  IO.print_array graph_v (IO.print_list (IO.print_pair print_int print_int)) ;
  IO.draw_graph oriented "red" graph_v [] ;
  if Sys.command "dot -Tps -o graph.ps graph.dot" = 0 
  then (if Sys.command "open graph.ps" = 1 then print_string "error open graph.ps")
  else print_string "error gnuplot" ;
  (graph_pred, graph_succ, graph_v) ;;

(* ========================================================================= *)
(*                          FONCTIONS DE PARCOURS                            *)
(* ========================================================================= *)

let rec breadth_first_search (graph_succ : 'a graph) (file_nodes : int DeQueue.struct_file) 
(file_parcours : int DeQueue.struct_file) tab_VNV : int DeQueue.struct_file =
  if DeQueue.get_first file_nodes = DeQueue.FNull 
  then ( match (Tools.choose_node_nv tab_VNV graph_succ 0) with
         | -1   -> file_parcours
         | node -> tab_VNV.(node) <- false ;
                   let f = (DeQueue.init ()) in 
                   (DeQueue.add node f) ;
                   (DeQueue.add node file_parcours) ;
                   (breadth_first_search graph_succ f file_parcours tab_VNV) )
  else ( let premier_node_file = (DeQueue.pop file_nodes) in
         ( try
             let x = (Tools.choose_neighbor_nv premier_node_file graph_succ tab_VNV) in
             tab_VNV.(x) <- false ;
             (DeQueue.add x file_parcours) ;
             (DeQueue.add x file_nodes)
           with Tools.Close_Node -> begin 
                                      tab_VNV.(DeQueue.pop file_nodes) <- false ;
                                      (DeQueue.delete_head file_nodes)  
                                    end ) ;
        (breadth_first_search graph_succ file_nodes file_parcours tab_VNV) ) ;; 

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
  d.(root) <- (0, None) ;
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
   
      if dy > dx + c then d.(y) <- (dx + c, (Some x)) ;
      lst_arc := (List.tl (!lst_arc)) ;
    done ;
  done ;
  let lst_arc = ref (Tools.get_lst_arc graph_succ 0) in
  while !lst_arc <> [] do
    let (x, y, c) = (List.hd (!lst_arc)) in
    let (dy, _) = d.(y) in
    let (dx, _) = d.(x) in
    if dy > dx + c then failwith "Il y a une boucle absorbante" ;
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
          Tools.delete_node_from_graph (graph_succ.(s)) s new_graph_pred ; 
          construct_list_topo new_graph_pred graph_succ [s] ) ;;
 
(*
 * Warning : the graph can't have circuit.
 *)

let bellman (lst_topo : 'a list) (graph_succ_v : 'a graph_valued) (graph_pred : 'a graph) (graph_succ : 'a graph) (root : 'a) : ('a arc) list =
  let n = Array.length graph_succ in
         let d = Array.make n (100, None) in (* +∞ *)
         d.(root) <- (0, None) ;
         for x = 0 to n - 1 do
           let lst_succ = (Tools.find_succ_topo graph_succ lst_topo x) in
           let lst_succ_cost = ref (Tools.assoc_cost_lst lst_succ graph_succ_v x) in
           while !lst_succ_cost <> [] do
             let (y, c) = (List.hd (!lst_succ_cost)) in
             let (dy, _) = d.(y) in 
             let (dx, _) = d.(x) in
             if dy > dx + c then d.(y) <- ((dx + c), (Some x)) ;
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
  d.(root) <- (0, true, None) ;
  let i = ref 0 in 
  while !i < n do 
    try
      let x = Tools.choose_open_dmin d in 
      let lst_succ = ref graph_succ_v.(x) in 
      while !lst_succ <> [] do 
        let (y, c) = (List.hd (!lst_succ)) in 
        let (dy, _, _) = d.(y) in 
        let (dx, _, _) = d.(x) in 
        if dy > dx + c then d.(y) <- ((dx + c), true, (Some x)) ; 
        lst_succ := (List.tl (!lst_succ)) ;
      done ;
      i := !i + 1 ; 
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

(* ========================================================================= *)
(*                      ARBRE COUVRANTS DE COÛT MINIMUM                      *)
(* ========================================================================= *)

(*
 * The graph has to be connexe, and not oriented
 *)

let prim (graph_succ_v : 'a graph_valued) (root : 'a) : ('a arc) list =
  let n = Array.length graph_succ_v in
  let d = Array.make n (100, false, None) in (* +∞ *)
  d.(root) <- (0, true, None) ;
  let i = ref 0 in 
  while !i < n do 
    try
      let x = Tools.choose_open_dmin d in 
      let lst_succ = ref graph_succ_v.(x) in 
      while !lst_succ <> [] do 
        let (y, c) = (List.hd (!lst_succ)) in 
        let (dy, b, f) = d.(y) in 
        if dy > c 
        then ( if (not b) && (f <> None) 
               then d.(x) <- (c, true, (Some y)) 
               else d.(y) <- (c, true, (Some x))) ;
        lst_succ := (List.tl (!lst_succ)) ;
      done ;
      i := !i + 1 ;
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
