
type 'a cell_file = { mutable prev : 'a elem_file ;
                      mutable next : 'a elem_file ;
                      info : 'a               }
and 'a elem_file =
    | FNull
    | Elem of 'a cell_file ;;

type 'a struct_file = { mutable first : 'a elem_file ;
                        mutable last : 'a elem_file  } ;;

exception Empty 

(* 
 * Return a new file, initially empty. 
 *)

let init () : 'a struct_file = {first = FNull ; last = FNull} ;;

let get_first (file : 'a struct_file) : 'a elem_file =
    file.first ;;

let get_last (file : 'a struct_file) : 'a elem_file =
    file.last ;;

(* 
 * add v f : adds the element v at the end of the file f.
 *)

let add (v : 'a) (f : 'a struct_file) : unit =
    match f.first with
    | FNull -> let rec elem = Elem {prev = FNull ; next = FNull ; info = v} in
                    f.first <- elem ; f.last <- elem 
    | _     -> let nlast = Elem {prev = f.last ; next = FNull ; info = v} in
               match f.last with
               | FNull  -> failwith 
                     "error, file.first <> FNull alors but file.last = FNull\n"
               | Elem x -> x.next <- nlast ; f.last <- nlast ;;

(* 
 * delete_head f : removes the first element in file f, or raises Empty if the file is empty.
 *)

let delete_head (f : 'a struct_file) : unit =
    match f.first with
    | FNull -> raise Empty
    | Elem x -> let nfirst = x.next in
                match nfirst with
                | FNull -> f.first <- FNull ; f.last <- FNull
                | Elem y -> let _ = y.prev <- FNull in
                            f.first <- nfirst ;; 

(*
 * top f : returns the first element in file f, without removing it from the file, or raises Empty if the file is empty.
 *)

let pop (f : 'a struct_file) : 'a =
    match f.first with
    | FNull  -> raise Empty
    | Elem x -> x.info ;; 


(* 
 * Construct a list from a file. 
 *)

let to_liste (f : 'a struct_file) : 'a list =
    match f.first with
    | FNull  -> []
    | Elem x -> ( let rec aux = function
                      | FNull -> []
                      | Elem y -> 
                        let elem = y.info  in 
                                    elem::(aux y.next)
                      in (aux f.first) ) ;; 

let print (f : 'a struct_file) : unit =
    match f.first with
    | FNull  -> print_string "[]\n"
    | Elem x -> (   let _ = print_string "[" in
                    let rec aux = function
                      | FNull -> Printf.printf "\b]\n"
                      | Elem y -> 
                        let _ = Printf.printf "%d " y.info  in 
                                    (aux y.next)
                      in (aux f.first) ) ;; 

