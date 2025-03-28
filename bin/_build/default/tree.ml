open Separator
open Graph


type graph_tree =
| Leaf of t
| Node of {
    left : graph_tree;
    separator : t;
    right : graph_tree;
    interface_left : (int * int) list;  (* edges between left and separator *)
    interface_right : (int * int) list; (* edges between right and separator *)
  }

let induced_subgraph (g : t) (nodes : int list) : t =
  let sub = create () in
  let node_set = Hashtbl.create (List.length nodes) in
  List.iter (fun id ->
    Hashtbl.add node_set id ();
    let _, (x, y) = Hashtbl.find g id in
    add_node sub id x y
  ) nodes;
  List.iter (fun id ->
    let neighbors = neighbors g id in
    List.iter (fun nbr ->
      if Hashtbl.mem node_set nbr then

        add_edge sub id nbr
    ) neighbors
  ) nodes;
  sub

let extract_interface_edges g part_nodes sep_nodes =
  let sep_set = Hashtbl.create (List.length sep_nodes) in
  List.iter (fun id -> Hashtbl.add sep_set id ()) sep_nodes;

  let edges = ref [] in
  List.iter (fun id ->
    let neighbors = neighbors g id in
    List.iter (fun nbr ->
      if Hashtbl.mem sep_set nbr then
        edges := (id, nbr) :: !edges
    ) neighbors
  ) part_nodes;
  !edges

let rec get_tree_from_graph g =
  let graph_size = Hashtbl.length g in
  if graph_size <= 10 then
    Leaf g
  else
    let separator, left, right = choose_separator g in
    let interface_left = extract_interface_edges g left separator in
    let interface_right = extract_interface_edges g right separator in
    Node {
      left = get_tree_from_graph (induced_subgraph g left);
      separator = induced_subgraph g separator;
      right = get_tree_from_graph (induced_subgraph g right);
      interface_left;
      interface_right;
    }

let reconstruct_graph (tree : graph_tree) : t =
  let g = create () in

  let rec add_all (subtree : graph_tree) =
    match subtree with
    | Leaf sg -> iter_nodes sg (fun id (x, y) -> add_node g id x y);
                  iter_edges sg (fun u v -> add_edge g u v)
    | Node { left; separator; right; interface_left; interface_right } ->
        add_all left;
        add_all right;
        iter_nodes separator (fun id (x, y) -> add_node g id x y);
        iter_edges separator (fun u v -> add_edge g u v);
        List.iter (fun (u, v) -> add_edge g u v) interface_left;
        List.iter (fun (u, v) -> add_edge g u v) interface_right
  in

  add_all tree;
  g

  let tree_to_dot (tree : graph_tree) =
    let rec aux tree count =
      match tree with
      | Leaf g ->
          let file = Printf.sprintf "data/leaf_%d.dot" count in
          to_dot_with_coords g file
      | Node { left; separator; right; interface_left; interface_right } ->
          (* Separator graph *)
          let file = Printf.sprintf "data/separator_%d.dot" count in
          to_dot_with_coords separator file;
          Printf.printf "interface_left: %d\n" (List.length interface_left);
          Printf.printf "interface_right: %d\n" (List.length interface_right);
  
          (* Recursively export left and right *)
          aux left (count * 2 + 1);
          aux right (count * 2 + 2)
    in
  
    aux tree 0