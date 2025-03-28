open Geograph.Graph
open Geograph.Separator
open Geograph.Embedder
open Geograph.Tree


let g1 = create_from_DIMAC "src/data/bremen_subgraph_300.gr" (*retreives the graph from the DIMAC file*)

let () =
  (* Random.init 42; *)
  (* Random.self_init (); *)
  (* let seed = int_of_float (Unix.gettimeofday () *. 1000.) in
  Random.init seed; *) (*initialises the random number generator, used for randomization in separators, deactivated for reproducibility*)
  fruchterman_reingold g1 1000; (*applies the fruchterman reingold algorithm to the graph*)
  scale_positions g1 0.1; (*scales the positions of the nodes in the graph*)
  let separator, left, right = choose_random_separators g1 in (*chooses a separator and splits the graph into two parts*)
  to_dot_with_partition_colors g1 "src/data/graph_partition.dot" separator left right; (*writes the graph in dot format to a file with partition colors, used for visualization of the separator*)

  let tg = get_tree_from_graph g1 50 in (*creates a tree from the graph*)

  let g' = reconstruct_graph tg in (*reconstructs the graph from the tree, used for testing purposes*)
  to_dot_with_coords g' "src/data/graph_rebuilt.dot";

  if check_graph_equality g1 g' then (*checks if the reconstructed graph is equal to the original graph, used for testing purposes*)
    Printf.printf "success\n"
  else
    Printf.printf "failure\n";

  let temp_sol = solve_tree tg in (*solves the tree, returns a solution*)
  let sol = global_pruning g1 temp_sol in (*prunes the solution on the graph*)

  to_dot_with_coords g1 "src/data/graph.dot"; (*writes the graph in dot format to a file*)
  to_dot_with_marked_vertices g1 "src/data/graph_marked.dot" sol; (*writes the graph in dot format to a file with marked vertices, used for visualization of the solution*)
  let dom = is_graph_dominated g1 sol in (*checks if the solution is correct, i.e. if it is a dominating set*)
  Printf.printf "dominated: %b\n" dom;
  Printf.printf "solution size: %d\n" (get_number_of_marked_vertices sol);
  Printf.printf "done\n"