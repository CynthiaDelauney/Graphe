(* FIXME :
 * type 'a arc = 'a Graph.arc ;;
 *)

type 'a arc = 'a * 'a ;;


let rec is_red_arc ((a, b) : 'a arc) (lst : ('a arc) list) : bool =
   List.exists (fun (a', b') -> (a = a' && b = b')) lst

(* ========================================================================= *)
(*                                 OUTPUT                                    *)
(* ========================================================================= *)

let output_int (oc : out_channel) (x : int) =
  output_string oc (string_of_int x) ;;

let rec output_node (oc : out_channel) (lst_red_arc : ('a arc) list) (node : 'a) 
(output_elem : out_channel -> 'a -> unit) : ('a list) -> unit = function
  | []   -> () 
  | a::q -> begin
              output_elem oc node ;
              output_string oc " -> " ;
              output_elem oc a ;
              if (is_red_arc (node, a) lst_red_arc)
              then output_string oc "[label = \"1\", color=red];\n" 
              else output_string oc ";\n" ;
              (output_node oc lst_red_arc node output_elem q)
            end ;;

let rec output_node_v (oc : out_channel) (lst_red_arc : ('a arc) list) (node : 'a) 
(output_elem : out_channel -> 'a -> unit) : (('a * int) list) -> unit = function
  | []   -> () 
  | (a, c)::q -> begin
                   output_elem oc node ;
                   output_string oc " -> " ;
                   output_elem oc a ;
                   if (is_red_arc (node, a) lst_red_arc)
                   then begin
                          output_string oc "[label = \"" ;
                          output_int oc c ;
                          output_string oc "\", color=red];\n" ;
                        end
                   else begin 
                          output_string oc "[label = \"" ;
                          output_int oc c ;
                          output_string oc "\"];\n" ;
                        end ;
                   (output_node_v oc lst_red_arc node output_elem q)
                 end ;;

let output_graph (oc : out_channel) ?graph ?graph_v (lst_red_arc : ('a arc) list) 
(output_elem : out_channel -> 'a -> unit) : unit =
  let _ = match graph, graph_v with
  | None, None     -> failwith "Aucun graphe donné en paramètre"
  | None, Some b   -> for i = 0 to (Array.length b) - 1 do 
                        output_node_v oc lst_red_arc i output_elem b.(i)
                      done  ; 
  | Some a, None   -> for i = 0 to (Array.length a) - 1 do 
                        output_node oc lst_red_arc i output_elem a.(i)
                      done  ;
  | Some a, Some b -> failwith "Deux graphes sont passés en paramètre" in
  output_string oc "0 [label=\"départ\"];\n" ;
  output_string oc "}" ;;

let output_int = function oc -> function x ->
    output_string oc (string_of_int x) ;;

let output_float oc f = output_string oc (string_of_float f) ;;

let output_ligne oc i e1 e2 e3 =
    begin 
        output_int oc i ;
        output_string oc " " ;
        output_float oc e1 ;
        output_string oc " " ;
        output_float oc e2 ;
        output_string oc " " ;
        output_float oc e3 ;
        output_string oc "\n"
    end ;;

(* ========================================================================= *)
(*                             PRINT FUNTIONS                                *)
(* ========================================================================= *)

let print_pair (print_elem : 'a -> unit) (x : 'a * 'a) : unit =
  begin 
    print_string "(" ;
    print_elem (fst x) ;
    print_string ", " ;
    print_elem (snd x);
    print_string ")" ;
  end ;;

let print_triple (print_elem : 'a -> unit) (x : 'a * 'a * 'a) : unit =
  let (x1, x2, x3) = x in
    begin 
      print_string "(" ;
      print_elem x1 ;
      print_string ", " ;
      print_elem x2;
      print_string ", " ;
      print_elem x3;
      print_string ")" ;
    end ;;

let print_array (tab : 'a array) (print_elem : 'a -> unit) =
    let _ = print_newline () in
    match (Array.length tab) with 
    | 0 -> Printf.printf "[||]\n" 
    | 1 -> Printf.printf "[| " ; print_elem tab.(0) ; Printf.printf " |]\n" 
    | t -> let _ = Printf.printf "[| " in
           let _ = print_elem tab.(0) in
           let _ = Printf.printf ";\n" in
             for i = 1 to t - 2 do 
               let _ = Printf.printf "   " in
               let _ = print_elem tab.(i) in
                 Printf.printf ";\n" 
             done ;
             Printf.printf "   ";
             print_elem tab.(t - 1);
             Printf.printf " |]\n" ;;

let print_list (print_elem : 'a -> unit) (lst : 'a list)  =
    if lst = [] 
    then Printf.printf "[]"
    else (
    let _ = Printf.printf "[" in
        let rec aux = function
            | [] -> failwith "No warning"
            | a::[] -> begin print_elem a ; Printf.printf "]" end
            | a::q  -> let _ = (print_elem a ; print_string "; ") in (aux q) 
        in 
            (aux lst)) ;;

let draw_graph graph_v (lst_red_arc : ('a arc) list) =
  let oc = open_out "graph.dot" in
    output_string oc "digraph G {" ;
    output_string oc "\n" ;
    output_graph oc ~graph_v:graph_v lst_red_arc output_int ;
    close_out oc ;;