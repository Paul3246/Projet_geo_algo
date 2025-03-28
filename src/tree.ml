open Separator
open Dom_SAT
open Graph


type graph_tree = (* type of a tree representing a graph *)
| Leaf of t
| Node of {
    left : graph_tree;
    separator : t;
    right : graph_tree;
    interface_left : (int * int) list;  (* edges between left and separator *)
    interface_right : (int * int) list; (* edges between right and separator *)
  }

let induced_subgraph (g : t) (nodes : int list) : t = (*creates an induced subgraph from a list of nodes*)
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

let extract_interface_edges g part_nodes sep_nodes = (*extracts the interface edges between two sets of nodes, usually a separator and a part*)
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

let rec get_tree_from_graph g precision= (*creates a tree from a graph, stops when the sizes of the subgraphs are less than or equal to the precision*)
  let graph_size = Hashtbl.length g in
  if graph_size <= precision then
    Leaf g
  else
    let separator, left, right = choose_random_separators g in
    let interface_left = extract_interface_edges g left separator in
    let interface_right = extract_interface_edges g right separator in
    Node {
      left = get_tree_from_graph (induced_subgraph g left) precision;
      separator = induced_subgraph g separator;
      right = get_tree_from_graph (induced_subgraph g right) precision;
      interface_left;
      interface_right;
    }

let reconstruct_graph (tree : graph_tree) : t = (*reconstructs a graph from a tree*)
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

let tree_to_dot (tree : graph_tree) = (*creates a dot file for every subgraph and separator in the tree*)
  let rec aux tree count =
    match tree with
    | Leaf g ->
        let file = Printf.sprintf "src/data/leaf_%d.dot" count in
        to_dot_with_coords g file
    | Node { left; separator; right; interface_left = _ ; interface_right = _ } ->
        (* Separator graph *)
        let file = Printf.sprintf "src/data/separator_%d.dot" count in
        to_dot_with_coords separator file;
        (* Recursively export left and right *)
        aux left (count * 2 + 1);
        aux right (count * 2 + 2)
  in

  aux tree 0

let extract_sol (model : (string * string) list) : (int * bool) list = (*extracts the solution from the model*)
  model
  |> List.fold_left (fun acc (name, value) ->
        if String.length name > 1 && name.[0] = 'x' then
          try
            let id = int_of_string (String.sub name 1 (String.length name - 1)) in
            (id, value = "true") :: acc
          with Failure _ -> acc  (* Ignore badly formatted names *)
        else
          acc
      ) []
  |> List.sort (fun (id1, _) (id2, _) -> compare id1 id2)

let get_number_of_marked_vertices (marked: (int * bool) list) : int = (*returns the number of marked vertices in the solution*)
  List.fold_left (fun acc (_, value) ->
    if value then
      acc + 1
    else
      acc
  ) 0 marked

let prune_separator_nodes (g : Graph.t) (merged_sol : (int * bool) list) (separator : Graph.t): (int * bool) list = (*prunes the separator nodes from the solution, to optimize the solution locally*)
  let tbl = Hashtbl.create 100 in

  (* Initialize the merged solution *)
  List.iter (fun (id, b) -> Hashtbl.replace tbl id b) merged_sol;

  (* Iterate over all node IDs in the separator subgraph *)
  Hashtbl.iter (fun id _ ->
    match Hashtbl.find_opt tbl id with
    | Some true ->
        Hashtbl.replace tbl id false;
        let candidate =
          Hashtbl.fold (fun id b acc -> (id, b) :: acc) tbl []
        in
        if not (is_graph_dominated g candidate) then
          Hashtbl.replace tbl id true  (* undo if needed *)
    | _ -> ()
  ) separator;

(* Return final pruned solution *)
Hashtbl.fold (fun id b acc -> (id, b) :: acc) tbl []


let global_pruning (g : Graph.t) (sol : (int * bool) list) : (int * bool) list = (*prunes the solution globally, by removing nodes that are not necessary*)
  let tbl = Hashtbl.create 100 in

  (* Initialize the merged solution *)
  List.iter (fun (id, b) -> Hashtbl.replace tbl id b) sol;

  (* Iterate over all TRUE nodes in the solution, and try removing them *)
  Hashtbl.iter (fun id _ ->
    match Hashtbl.find_opt tbl id with
    | Some true ->
        Hashtbl.replace tbl id false;
        let candidate =
          Hashtbl.fold (fun id b acc -> (id, b) :: acc) tbl []
        in
        if not (is_graph_dominated g candidate) then
          Hashtbl.replace tbl id true  (* undo if removal breaks domination *)
    | _ -> ()
  ) g;

  (* Return pruned solution as list *)
  Hashtbl.fold (fun id b acc -> (id, b) :: acc) tbl []

let switch_node (g : t) (id : int) (sol : (int * bool) list) : ((int * bool) list * int) = (*switches a node from unmarked to marked, and switches its neighbors to unmarked*)
  let switch_count = ref 0 in
  let solhash = Hashtbl.create 100 in
  List.iter (fun (id, b) -> Hashtbl.replace solhash id b) sol;
  let value = Hashtbl.find solhash id in
  let nbrs = neighbors g id in
  if not value then begin
    (* Check if it dominates any marked neighbors *)
    let has_marked_neighbor = List.exists (fun nbr ->
      Hashtbl.find_opt solhash nbr = Some true
    ) nbrs in
    if not has_marked_neighbor then ([], 0)
    else begin
      Hashtbl.replace solhash id true;
      List.iter (fun nbr ->
        match Hashtbl.find_opt solhash nbr with
        | Some true -> begin (* switch to false *)
            Hashtbl.replace solhash nbr false;
            incr switch_count
          end
        | _ -> ()
      ) nbrs;
      let new_sol = Hashtbl.fold (fun id b acc -> (id, b) :: acc) solhash [] in
      (new_sol, !switch_count)
    end
  end else
    begin
    ([], 0)
    end

(*Applies a switching round to the solution, keeps the switch if it gives a valid and better solution,
returns a solution and a boolean indicating if the solution has changed*)
let try_switch_once (g : t) (sol : (int * bool) list) : (int * bool) list * bool =
  let sol_len = get_number_of_marked_vertices sol in
  let changed = ref false in
  let rec aux acc sol =
    match sol with
    | [] -> List.rev acc
    | (id, b) :: tl ->
      (* Printf.printf "Switching node %d\n" id; *)
        let new_sol, switch_count = switch_node g id sol in
        let new_len = sol_len + 1 - switch_count in
        if new_len < sol_len && is_graph_dominated g new_sol then begin
          changed := true;
          aux [] new_sol (* restart from scratch *)
        end else
          aux ((id, b) :: acc) tl
  in
  aux [] sol, !changed

let try_switch_fixpoint (g : t) (sol : (int * bool) list) : (int * bool) list = (*applies a switching round until no more switches can be made, uses a fixpoint convergence*)
  let rec fix sol =
    let new_sol, changed = try_switch_once g sol in
    if changed then fix new_sol else new_sol
  in
  fix sol

let merge_solutions (solright : (int * bool) list) (solleft : (int * bool) list) (sol_sep : (int * bool) list): (int * bool) list = (*merges the solutions of the left and right subgraphs with the separator solution*)
  let merged = Hashtbl.create 100 in

  let update id b =
    let prev = Hashtbl.find_opt merged id in
    match prev with
    | Some true -> ()  (* already true, do nothing *)
    | _ -> Hashtbl.replace merged id b
  in

  List.iter (fun (id, b) -> update id b) solright;
  List.iter (fun (id, b) -> update id b) solleft;
  List.iter (fun (id, b) -> update id b) sol_sep;

  Hashtbl.fold (fun id b acc -> (id, b) :: acc) merged []

let fuse (g1 : t) (g2 : t) (sep : t) (interface_left : (int * int) list) (interface_right : (int * int) list) : t = (*fuses two graphs and a separator, and adds interface edges*)
  let g = create () in

  (* Add nodes and edges from g1 *)
  iter_nodes g1 (fun id (x, y) -> add_node g id x y);
  iter_edges g1 (fun u v -> add_edge g u v);

  (* Add nodes and edges from g2 *)
  iter_nodes g2 (fun id (x, y) -> add_node g id x y);
  iter_edges g2 (fun u v -> add_edge g u v);

  (* Add nodes and edges from separator *)
  iter_nodes sep (fun id (x, y) -> add_node g id x y);
  iter_edges sep (fun u v -> add_edge g u v);

  (* Add interface edges *)
  List.iter (fun (u, v) -> add_edge g u v) interface_left;
  List.iter (fun (u, v) -> add_edge g u v) interface_right;

  g

let check_nodes_in_graph (g : t) (nodes : int list) : bool = (*checks if a list of nodes is in the graph, used for debugging*)
  List.for_all (fun id -> Hashtbl.mem g id) nodes

let extract_nodes_from_solution (sol : (int * bool) list) : int list = (*extracts the nodes from a solution, used for debugging*)
  List.fold_left (fun acc (id, _) ->
    id :: acc
  ) [] sol


(*solves the minimum dominating set problem with a tree decomposition, using a recursive divide and conquer approach,
binary search SAT on the leaves, and improves the solution with local and global optimizations, returns an approximate solution*)
let solve_tree (tree : graph_tree) : (int * bool) list =
  let rec solve_tree' tree =
    match tree with
    | Leaf g ->
      let temp_sol = binary_search_MDS g in
      extract_sol temp_sol, g
    | Node { left; separator; right; interface_left; interface_right } ->
        (*solves the left and right trees, and the separator subgraph*)
        let sol_left, g_left = solve_tree' left in
        let sol_right, g_right = solve_tree' right in
        let temp_sol_sep = binary_search_MDS separator in
        let sol_sep = extract_sol temp_sol_sep in
        Printf.printf "Separator size %d\n" (Hashtbl.length separator);

        (*fuses the left and right subgraphs with the separator, and merges the solutions*)
        let g' = fuse g_left g_right separator interface_left interface_right in
        let merged = merge_solutions sol_right sol_left sol_sep in

        (*prunes the solution locally on separator nodes in the merged solution*)
        let pruned = prune_separator_nodes g' merged separator in
        
        (*checks if the pruned solution is not good enough, and reoptimizes if needed*)
        let pruned_size = List.length pruned in
        let subgraph_size = Hashtbl.length g' in
        let ratio = float_of_int pruned_size /. float_of_int subgraph_size in
        let should_reoptimize = ratio < 2. && subgraph_size < 40 in

        (*optimizes the solution with a more global exact solution finding if needed only for small subgraphs, avoids large solutions by using a bounded binary search*)
        let final_sol =
          if should_reoptimize then (
            Printf.printf "Reoptimizing with size %d\n" pruned_size;
            let alt_sol = binary_search_MDS_less_r g' pruned_size in
            extract_sol alt_sol
          ) else (
            pruned
          )
        in
        final_sol, g'
  in
  (* retrieves the solution and the final graph, and apply two global optimizations: global pruning and a fixpoint convergence switching*)
  let sol, graph = solve_tree' tree in
  let final_sol = try_switch_fixpoint graph sol in
  global_pruning graph final_sol