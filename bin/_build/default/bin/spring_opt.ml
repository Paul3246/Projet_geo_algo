open Geograph

let g1 = Graph.create_from_DIMAC "data/bremen_subgraph_50.gr"
let () =
  Embedder.fruchterman_reingold g1 1000;
  Embedder.scale_positions g1 0.1;
  let separator, left, right = Separator.choose_separator g1 in
  Graph.to_dot_with_partition_colors g1 "data/graph.dot" separator left right;
  let tg = Tree.get_tree_from_graph g1 in
  Tree.tree_to_dot tg;
  let g' = Tree.reconstruct_graph tg in
  Graph.to_dot_with_coords g' "data/graph_rebuilt.dot";
  if Graph.check_graph_equality g1 g' then
    Printf.printf "success\n"
  else
    Printf.printf "failure\n";



