(* FIXME :
 * type 'a arc = 'a Graph.arc ;;
 *)

type 'a arc = 'a * 'a ;;

(* ========================================================================= *)
(*                                 OUTPUT                                    *)
(* ========================================================================= *)

let output_int (oc : out_channel) (x : int) =
  output_string oc (string_of_int x) ;;

let rec output_node (oc : out_channel) (oriented : bool) (color : string) (lst_red_arc : ('a arc) list) (node : 'a) 
(output_elem : out_channel -> 'a -> unit) : ('a list) -> unit = function
  | []   -> () 
  | a::q -> begin
              output_elem oc node ;
              if oriented then output_string oc " -> " 
                          else output_string oc "--" ;
              output_elem oc a ;
              if List.exists (fun (a', b') -> (node = a' && a = b')) lst_red_arc
              then begin 
                     output_string oc "[label = \"1\", color=" ; 
                     output_string oc color ; 
                     output_string oc "];\n" ;
                   end
              else output_string oc ";\n" ;
              (output_node oc oriented color lst_red_arc node output_elem q)
            end ;;

let rec output_node_v (oc : out_channel) (oriented : bool) (color : string) (lst_red_arc : ('a arc) list) (node : 'a) 
(output_elem : out_channel -> 'a -> unit) : (('a * int) list) -> unit = function
  | []   -> () 
  | (a, c)::q -> begin
                   output_elem oc node ;
                   if oriented then output_string oc " -> " 
                          else output_string oc "--" ;
                   output_elem oc a ;
                   if List.exists (fun (a', b') -> (node = a' && a = b')) lst_red_arc
                   then begin
                          output_string oc "[label = \"" ;
                          output_int oc c ;
                          output_string oc "\", color=" ;
                          output_string oc color ; 
                          output_string oc "];\n" ;
                        end
                   else begin 
                          output_string oc "[label = \"" ;
                          output_int oc c ;
                          output_string oc "\"];\n" ;
                        end ;
                   (output_node_v oc oriented color lst_red_arc node output_elem q)
                 end ;;

let output_graph (oc : out_channel) (oriented : bool) (color : string) ?graph ?graph_v (lst_red_arc : ('a arc) list) 
(output_elem : out_channel -> 'a -> unit) : unit =
  let _ = match graph, graph_v with
  | None, None     -> (List.iter (fun (a, b) -> begin output_elem oc a ; 
                                                      output_string oc " -> " ;
                                                      output_elem oc b ; 
                                                      output_string oc "[color=" ; 
                                                      output_string oc color ; 
                                                      output_string oc "];\n" ; 
                                                end ) lst_red_arc) ;
  | None, Some b   -> for i = 0 to (Array.length b) - 1 do 
                        output_node_v oc oriented color lst_red_arc i output_elem b.(i)
                      done  ; 
  | Some a, None   -> for i = 0 to (Array.length a) - 1 do 
                        output_node oc oriented color lst_red_arc i output_elem a.(i)
                      done  ;
  | Some a, Some b -> failwith "Deux graphes sont passés en paramètre" in
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

let print_bool (b : bool) = print_string (string_of_bool b)

let print_option (print_elem : 'a -> unit) (op : 'a option) = 
  match op with | None -> print_string "None" 
                | Some x -> print_elem x ;; 

let print_pair (print_elem_a : 'a -> unit) (print_elem_b : 'b -> unit) (x : 'a * 'b) : unit =
  begin 
    print_string "(" ;
    print_elem_a (fst x) ;
    print_string ", " ;
    print_elem_b (snd x);
    print_string ")" ;
  end ;;

let print_triple (print_elem_a : 'a -> unit) (print_elem_b : 'b -> unit) (print_elem_c : 'c -> unit) 
(x : 'a * 'b * 'c) : unit =
  let (x1, x2, x3) = x in
    begin 
      print_string "(" ;
      print_elem_a x1 ;
      print_string ", " ;
      print_elem_b x2;
      print_string ", " ;
      print_elem_c x3;
      print_string ")" ;
    end ;;

let print_array (tab : 'a array) (print_elem : 'a -> unit) =
    let _ = print_newline () in
    match (Array.length tab) with 
    | 0 -> Printf.printf "[||]\n" 
    | 1 -> Printf.printf "[| " ; print_elem tab.(0) ; Printf.printf " |]\n" 
    | t -> Printf.printf "[| " ;
           print_elem tab.(0) ;
           Printf.printf ";\n" ;
           for i = 1 to t - 2 do 
             Printf.printf "   " ;
             print_elem tab.(i) ;
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
            | a::[] -> print_elem a ; Printf.printf "]" 
            | a::q  -> print_elem a ; print_string "; " ; (aux q) 
        in 
            (aux lst)) ;;

let draw_graph (oriented : bool) (color : string) graph_v (lst_red_arc : ('a arc) list) =
  let oc = open_out "graph.dot" in
    if oriented 
    then output_string oc "digraph G {" 
    else output_string oc "graph G {" ;
    output_string oc "\n" ;
    output_graph oc oriented color ~graph_v:graph_v lst_red_arc output_int ;
    close_out oc ;;

let draw_arbo (color : string) (lst_red_arc : ('a arc) list) =
  let oc = open_out "arbo.dot" in
    output_string oc "digraph G {" ;
    output_string oc "\n" ;
    output_graph oc true color lst_red_arc output_int ;
    close_out oc ;;