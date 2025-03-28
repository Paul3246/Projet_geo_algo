(* Module for spring embedding of graphs without using the Barnes-Hut approximation, not used in the final implementation *)

module Sgraph = struct

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

    let init_nodes_random_box g w l =
      Random.init 42;
      let centerx = w /. 2.0 in
      let centery = l /. 2.0 in
      let rand_offset () = (Random.float w) -. centerx in
      let rand_offset_y () = (Random.float l) -. centery in
      Hashtbl.iter (fun id (adj, _) ->
        Hashtbl.replace g id (adj, (rand_offset (), rand_offset_y ()))
      ) g

    let init_k g w l=
      let a = area w l in
      let n = float (Hashtbl.length g) in
      sqrt (a /. n)

    let f_repul k d =
      k ** 2. /. d

    let f_attract k d =
      d ** 2. /. k

    let compute_forces k g =
      let forces = Hashtbl.create (Hashtbl.length g) in
    
      (* Initialize displacement vectors for each node *)
      Hashtbl.iter (fun id _ -> Hashtbl.add forces id (0., 0.)) g;
    
      (* Compute repulsive forces *)
      Hashtbl.iter (fun id1 (_, (x1, y1)) ->
        Hashtbl.iter (fun id2 (_, (x2, y2)) ->
          if id1 != id2 then
            let deltax, deltay = x1 -. x2, y1 -. y2 in
            let normd = sqrt (deltax ** 2. +. deltay ** 2.) +. 1e-9 in
            let fnormd = f_repul k normd in
            let fx, fy = (deltax /. normd) *. fnormd, (deltay /. normd) *. fnormd in
            let vdispx, vdispy = Hashtbl.find forces id1 in
            Hashtbl.replace forces id1 (vdispx +. fx, vdispy +. fy)
        ) g
      ) g;
    
      (* Compute attractive forces using existing displacement vectors *)
      let edges = get_edges g in
      Hashtbl.iter (fun (id1, id2) _ ->
        let vx, vy = x_y g id1 in
        let ux, uy = x_y g id2 in
        let deltax, deltay = vx -. ux, vy -. uy in
        let normd = sqrt (deltax ** 2. +. deltay ** 2.) +. 1e-9 in
        let fnormd = f_attract k normd in
        let fx, fy = (deltax /. normd) *. fnormd, (deltay /. normd) *. fnormd in
    
        (* Update displacement vectors (accumulate forces) *)
        let vdispx, vdispy = Hashtbl.find forces id1 in
        let udispx, udispy = Hashtbl.find forces id2 in
        Hashtbl.replace forces id1 (vdispx -. fx, vdispy -. fy);
        Hashtbl.replace forces id2 (udispx +. fx, udispy +. fy)
      ) edges;
    
      forces

    let scale_positions g factor =
      Hashtbl.iter (fun id (adj, (x, y)) ->
        Hashtbl.replace g id (adj, (x *. factor, y *. factor))
      ) g

    let cool g temp =
      let n = float_of_int (Hashtbl.length g) in
      let cooling_factor =
        if n <= 20. then 0.995
        else if n >= 100. then 0.99995
        else 0.995 +. ((0.99995 -. 0.995) *. ((n -. 20.) /. 80.))
      in
      temp *. cooling_factor

    let fruchterman_reingold g max_iter=
      let n = Hashtbl.length g in
      let w = 10. *. sqrt (float n) in
      let l = 10. *. sqrt (float n) in
      init_nodes_random_box g w l;
      let k = init_k g w l in
      let temp = ref 1. in
      let threshold = 1e-10 in
      let curr_iter = ref 0 in
      while !temp > threshold && !curr_iter < max_iter do
        let forces = compute_forces k g in
        Hashtbl.iter (fun id1 (adj, _) ->
          let x, y = x_y g id1 in
          let fx, fy = Hashtbl.find forces id1 in
          let normf = sqrt (fx ** 2. +. fy ** 2.) +. 1e-9 in
          let step = min normf !temp in
          let newx = x +. (fx /. normf) *. step in
          let newy = y +. (fy /. normf) *. step in
          (* let finalx = min (w /. 2.) (max (-. w /. 2.) newx) in
          let finaly = min (l /. 2.) (max (-. l /. 2.) newy) in *)
          Hashtbl.replace g id1 (adj, (newx, newy))
        ) g;
        temp := cool g !temp;
        incr curr_iter
      done;
      Printf.printf "Converged after %d iterations\n" !curr_iter

end

let g1 = Sgraph.create_from_DIMAC "data/bremen_subgraph_300.gr"
let () =
  Sgraph.to_dot g1 "easy.dot";
  Sgraph.fruchterman_reingold g1 1000;
  Sgraph.scale_positions g1 0.1;
  Sgraph.to_dot_with_coords g1 "Seasy.dot"