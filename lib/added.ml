type 'a t = Node of 'a | List of 'a t list | Arg of 'a t Arg.t | None

let to_list = function
  | List l -> l
  | Node _ as n -> [n]
  | None -> []
  | _ -> failwith "Added.to_list: Wrong added type"

let to_option = function Node n -> Some n | None -> None | _ -> failwith "Added.to_option: Wrong added type"

let to_arg = function Arg a -> a | _ -> failwith "Added.to_arg: Wrong added type"

let to_string a =
  let tab n = String.make n ' ' in
  let concat l = String.concat "\n" l ^ "\n" in
  let rec string_of_added_rec n a =
    tab n ^ match a with
      | Node _   -> "Anode <>"
      | List l   -> "Alist <\n" ^ concat (List.map (string_of_added_rec (n+1)) l) ^ tab n ^ ">"
      | Arg a    -> "Aarg <\n" ^ concat (List.map (string_of_added_rec (n+1)) (Arg.to_list a)) ^ tab n ^ ">"
      | None     -> "Anone" in
  string_of_added_rec 0 a

let to_node = function
  | Node n -> n
  | a -> failwith [%string "Added.to_node: Wrong added type. Got %{to_string a}."]

let rec flatten = function
  | Node n -> [n]
  | List l -> List.flatten (List.map flatten l)
  | None -> []
  | Arg _ -> failwith "???"

let rec map f = function
  | Node n -> Node (f n)
  | List l -> List (List.map (map f) l)
  | None -> None
  | Arg _ -> failwith "???"

let rec node_map f = function
  | Node _ as n -> f n
  | List l -> List (List.map (node_map f) l)
  | None -> None
  | Arg _ -> failwith "???"
