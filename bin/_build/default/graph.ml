type t =  (int, (int, unit) Hashtbl.t * (float * float)) Hashtbl.t

let iter_nodes g f =
  Hashtbl.iter (fun id (_, (x, y)) ->
    f id (x, y)
  ) g

let iter_edges g f =
  Hashtbl.iter (fun u (adj_u, _) ->
    Hashtbl.iter (fun v _ ->
      if u < v then f u v
    ) adj_u
  ) g

(* let laplacian_matrix g =
  let n = Hashtbl.length g in
  let l = Mat.create n n in
  for i = 1 to n do
    match Hashtbl.find_opt g i with
    | Some (adj, _) ->
        let deg = float (Hashtbl.length adj) in
        Bigarray.Array2.set l i i deg;
        Hashtbl.iter (fun j _ ->
          Bigarray.Array2.set l i j (-1.)
        ) adj
    | None -> ()
  done;
  l

let print_laplacian l =
  let n = Mat.dim1 l in
  for i = 1 to n do
    for j = 1 to n do
      Printf.printf "%f " (Bigarray.Array2.get l i j)
    done;
    Printf.printf "\n"
  done

let copy_matrix m=
  let rows = Bigarray.Array2.dim1 m in
  let cols = Bigarray.Array2.dim2 m in
  let m_copy = Mat.create rows cols in
  for i = 1 to rows do
    for j = 1 to cols do
      Bigarray.Array2.set m_copy i j (Bigarray.Array2.get m i j)
    done
  done;
  m_copy

  let fiedler_vector l =
    let n = Bigarray.Array2.dim1 l in
    let eigvals = Vec.create n in
    let eigvecs = copy_matrix l in
    ignore (syev ~vectors:true ~w:eigvals eigvecs);
    (* Fiedler vector = second eigenvector (column 2) *)
    let fiedler = Array.make n 0.0 in
    for i = 1 to n do
      fiedler.(i - 1) <- Bigarray.Array2.get eigvecs i 2
    done;
    fiedler *)
    
type quadtree =
  | Empty
  | Leaf of float * float * int (* (x, y, node_id) *)
  | Node of float * float * float * quadtree * quadtree * quadtree * quadtree (* center_x, center_y, mass, NW, NE, SW, SE *)

let rec insert_quadtree qt x y id =
  match qt with
  | Empty -> Leaf (x, y, id)
  | Leaf (lx, ly, lid) ->
      let cx = (x +. lx) /. 2. in
      let cy = (y +. ly) /. 2. in
      Node (cx, cy, 2., insert_quadtree Empty lx ly lid, insert_quadtree Empty x y id, Empty, Empty)
  | Node (cx, cy, mass, nw, ne, sw, se) ->
      let mass' = mass +. 1. in
      let nw', ne', sw', se' =
        if x < cx then
          if y < cy then insert_quadtree nw x y id, ne, sw, se
          else nw, ne, insert_quadtree sw x y id, se
        else
          if y < cy then nw, insert_quadtree ne x y id, sw, se
          else nw, ne, sw, insert_quadtree se x y id
      in
      Node (cx, cy, mass', nw', ne', sw', se')

let create () : t = Hashtbl.create 100

let add_node g id lat lon = Hashtbl.add g id (Hashtbl.create 10, (lat, lon))

let add_edge g id1 id2 =
  match Hashtbl.find_opt g id1, Hashtbl.find_opt g id2 with
  | Some (adj1, _), Some (adj2, _) ->
    Hashtbl.replace adj1 id2 ();
    Hashtbl.replace adj2 id1 ()
  | _ -> raise (Invalid_argument "add_edge: node not found")

let neighbors g id =
  match Hashtbl.find_opt g id with
  | Some (adj, _) -> Hashtbl.fold (fun k _ acc -> k :: acc) adj []
  | _ -> raise (Invalid_argument "neighbors: node not found")

let x_y g id =
  match Hashtbl.find_opt g id with
  | Some (_, x_y) -> x_y
  | _ -> raise (Invalid_argument "x_y: node not found")

let add_n_nodes g n =
  for i = 1 to n do
    add_node g i 1. 1.
  done

let degree g id =
  match Hashtbl.find_opt g id with
  | Some (adj, _) -> Hashtbl.length adj
  | _ -> raise (Invalid_argument "degree: node not found")

let get_edges g =
  let edges = Hashtbl.create 100 in
  Hashtbl.iter (fun id (adj, _) ->
    Hashtbl.iter (fun k _ ->
      if id < k then
        Hashtbl.add edges (id, k) ()
    ) adj
  ) g;
  edges

let create_from_DIMAC (filename : string) : t =
  let g = create () in
  let ic = open_in filename in
  try
    while true do
      let line = input_line ic in
      match String.split_on_char ' ' line with
      | "c" :: _ -> ()
      | "p" :: "ds" :: n :: _ :: _ -> add_n_nodes g (int_of_string n)
      | id1 :: id2 :: _ -> add_edge g (int_of_string id1) (int_of_string id2)
      | _ -> ()
    done;
    g
  with
  |End_of_file ->
    close_in ic;
    g
  |e ->
    close_in_noerr ic;
    raise e

let print g =
  Hashtbl.iter (fun id (adj, _) ->
      Printf.printf "node %d:  \n" id;
      Hashtbl.iter (fun k _ -> Printf.printf "  %d\n" k) adj
    ) g

let to_dot g filename =
  let oc = open_out filename in
  Printf.fprintf oc "graph G {\n";
  Hashtbl.iter (fun id (adj, _) ->
      Hashtbl.iter (fun k _ ->
          if id < k then
            Printf.fprintf oc "  %d -- %d;\n" id k
        ) adj
    ) g;
  Printf.fprintf oc "}\n";
  close_out oc

  let to_dot_with_coords g filename =
    let oc = open_out filename in
    Printf.fprintf oc "graph G {\n";
    Hashtbl.iter (fun id (adj, (lat, lon)) ->
        Printf.fprintf oc "  %d [pos=\"%f,%f!\"];\n" id lat lon;
        Hashtbl.iter (fun k _ ->
            if id < k then
              Printf.fprintf oc "  %d -- %d;\n" id k
          ) adj
      ) g;
    Printf.fprintf oc "}\n";
    close_out oc

  let to_dot_with_partition_colors g filename separator left right =
    let oc = open_out filename in
    Printf.fprintf oc "graph G {\n";
    Hashtbl.iter (fun id (adj, (lat, lon)) ->
        let color =
          if List.mem id separator then "red"
          else if List.mem id left then "lightblue"
          else if List.mem id right then "lightgreen"
          else "gray"
        in
        Printf.fprintf oc "  %d [pos=\"%f,%f!\", style=filled, fillcolor=%s];\n"
          id lat lon color;
        Hashtbl.iter (fun k _ ->
            if id < k then
              Printf.fprintf oc "  %d -- %d;\n" id k
          ) adj
      ) g;
    Printf.fprintf oc "}\n";
    close_out oc

let get_highest_degree_node g =
  Hashtbl.fold (fun id (adj, _) (max_id, max_deg) ->
    let deg = Hashtbl.length adj in
    if deg > max_deg then (id, deg) else (max_id, max_deg)
  ) g (0, 0) |> fst

let get_width g =
  let min_x = ref infinity in
  let max_x = ref neg_infinity in
  Hashtbl.iter (fun _ (_, (x, _)) ->
    if x < !min_x then min_x := x;
    if x > !max_x then max_x := x
  ) g;
  !max_x -. !min_x

let get_height g =
  let min_y = ref infinity in
  let max_y = ref neg_infinity in
  Hashtbl.iter (fun _ (_, (_, y)) ->
    if y < !min_y then min_y := y;
    if y > !max_y then max_y := y
  ) g;
  !max_y -. !min_y

let area w l =
  w *. l

let check_graph_equality g1 g2 =
  let eq = ref true in
  Hashtbl.iter (fun id1 (adj1, _) ->
    match Hashtbl.find_opt g2 id1 with
    | Some (adj2, _) ->
        if not (Hashtbl.length adj1 = Hashtbl.length adj2) then
          eq := false
        else
          Hashtbl.iter (fun id2 _ ->
            if not (Hashtbl.mem adj1 id2) then
              eq := false
          ) adj2
    | None -> eq := false
  ) g1;
  !eq