(*module made to implement tutte's algorithm, not used in the final implementation because of need for an outer face*)

type t =  (int, (int, unit) Hashtbl.t * (float * float)) Hashtbl.t

let create () : t = Hashtbl.create 100

let add_node g id lat lon = Hashtbl.add g id (Hashtbl.create 10, (lat, lon))

let add_edge g id1 id2 =
  match Hashtbl.find_opt g id1, Hashtbl.find_opt g id2 with
  | Some (adj1, _), Some (adj2, _) ->
    Hashtbl.add adj1 id2 ();
    Hashtbl.add adj2 id1 ()
  | _ -> raise (Invalid_argument "add_edge: node not found")

let neighbors g id =
  match Hashtbl.find_opt g id with
  | Some (adj, _) -> Hashtbl.fold (fun k _ acc -> k :: acc) adj []
  | _ -> raise (Invalid_argument "neighbors: node not found")

let lat_lon g id =
  match Hashtbl.find_opt g id with
  | Some (_, lat_lon) -> lat_lon
  | _ -> raise (Invalid_argument "lat_lon: node not found")

let add_n_nodes g n =
  for i = 1 to n do
    add_node g i 1. 1.
  done

let degree g id =
  match Hashtbl.find_opt g id with
  | Some (adj, _) -> Hashtbl.length adj
  | _ -> raise (Invalid_argument "degree: node not found")

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

  let get_highest_degree_node g =
    Hashtbl.fold (fun id (adj, _) (max_id, max_deg) ->
      let deg = Hashtbl.length adj in
      if deg > max_deg then (id, deg) else (max_id, max_deg)
    ) g (0, 0) |> fst
  

let get_outer_face g =
  let visited = Hashtbl.create (Hashtbl.length g) in

  let rec dfs path node =
    if List.mem node path then
      (* Cycle detected: Extract the cycle from the first occurrence of `node` *)
      let rec extract_cycle = function
        | [] -> []
        | x :: xs when x = node -> node :: x :: xs (* Include the closing node *)
        | _ :: xs -> extract_cycle xs
      in
      Some (List.rev (extract_cycle path))
    else begin
      Hashtbl.add visited node true;
      let neighbors_list = neighbors g node in
      List.fold_left (fun acc neighbor ->
        if acc <> None then acc (* Stop searching if a cycle was found *)
        else dfs (node :: path) neighbor
      ) None neighbors_list
    end
  in

  (* Start from any node *)
  match Hashtbl.fold (fun id _ acc -> match acc with None -> Some id | _ -> acc) g None with
  | None -> failwith "Graph is empty"
  | Some start_node ->
    match dfs [] start_node with
    | Some cycle -> cycle
    | None -> failwith "No cycle found"

  


let place_outer_face_on_circle g outer_face size =
  let radius = float size /. 2. in
  let center = radius in
  let n = List.length outer_face in
  let step = 2. *. Float.pi /. float_of_int n in
  List.iteri (fun i id ->
    let angle = float_of_int i *. step in
    let new_x = center +. (radius *. cos angle) in
    let new_y = center +. (radius *. sin angle) in
    let adj = Hashtbl.find g id |> fst in
    Hashtbl.replace g id (adj, (new_x,  new_y))
  ) outer_face

let initialize_interior g interior_nodes =
  List.iter (fun id ->
    let adj = Hashtbl.find g id |> fst in
    let neighbors = neighbors g id in
    let sum_x, sum_y =
      List.fold_left (fun (sx, sy) neighbor ->
        let (nlat, nlon) = lat_lon g neighbor in
        (sx +. nlat, sy +. nlon)
      ) (0., 0.) neighbors
    in
    let degv = List.length neighbors in
    let new_x = sum_x /. float_of_int degv in
    let new_y = sum_y /. float_of_int degv in
    Hashtbl.replace g id (adj, (new_x, new_y))
  ) interior_nodes

let tutte_coord g node =
  let neighbors = neighbors g node in
  let degv = degree g node in
  if degv = 0 then failwith "tutte_coord: node has no neighbors";
  let sum_x, sum_y =
    List.fold_left (fun (sx, sy) neighbor ->
      let (nlat, nlon) = lat_lon g neighbor in
      (sx +. nlat, sy +. nlon)
    ) (0., 0.) neighbors
  in
  (sum_x /. float_of_int degv, sum_y /. float_of_int degv)

let tutte_iter g threshold inner_nodes max_iter=
  let max_diff = ref (threshold +. 1.) in
  let iter = ref 0 in
  while !max_diff > threshold && !iter < max_iter do
    max_diff := 0.;
    List.iter (fun id ->
      let (x, y) = tutte_coord g id in
      let (adj, (old_x, old_y)) = Hashtbl.find g id in
      let diff = sqrt ((x -. old_x) ** 2. +. (y -. old_y) ** 2.) in
      if diff > !max_diff then max_diff := diff;
      Hashtbl.replace g id (adj, (x, y))
    ) inner_nodes;
    incr iter;
  done

let geometrical_embedding g =
  let size = 10 in
  let threshold = 1e-6 in
  let max_iter = 1000 in
  let outer_face = get_outer_face g in
  place_outer_face_on_circle g outer_face size;
  let inner_nodes = Hashtbl.fold (fun id _ acc ->
    if not (List.mem id outer_face) then id :: acc else acc
  ) g [] in
  initialize_interior g inner_nodes;
  tutte_iter g threshold inner_nodes max_iter