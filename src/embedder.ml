open Graph

let init_nodes_random_box g w l = (*initalises the position of nodes at a random position in a box of width w and length l*)
  Random.init 42;
  let centerx = w /. 2.0 in
  let centery = l /. 2.0 in
  let rand_offset () = (Random.float w) -. centerx in
  let rand_offset_y () = (Random.float l) -. centery in
  Hashtbl.iter (fun id (adj, _) ->
    Hashtbl.replace g id (adj, (rand_offset (), rand_offset_y ()))
  ) g

let init_k g w l= (*initialises the spring constant k*)
  let a = Graph.area w l in
  let n = float (Hashtbl.length g) in
  sqrt (a /. n)

let f_repul k d = (*repulsive force function*)
  k ** 2. /. d

let f_attract k d = (*attractive force function*)
  d ** 2. /. k

let theta = 0.351  (* Barnes-Hut approximation threshold *)

let rec compute_repulsive_force qt x y k = (*computes the repulsive force on a node at position x, y*)
  match qt with
  | Empty -> (0., 0.)
  | Leaf (lx, ly, _) ->
      let dx, dy = x -. lx, y -. ly in
      let dist = sqrt (dx *. dx +. dy *. dy) +. 1e-9 in
      let force = f_repul k dist in
      (force *. dx /. dist, force *. dy /. dist)
  | Node (cx, cy, mass, nw, ne, sw, se) ->
      let dx, dy = x -. cx, y -. cy in
      let dist = sqrt (dx *. dx +. dy *. dy) +. 1e-9 in
      if dist /. mass < theta then
        (* Approximate the whole cluster as a single node *)
        let force = f_repul k dist in
        (force *. dx /. dist, force *. dy /. dist)
      else
        (* Recursively compute forces from children *)
        let fx1, fy1 = compute_repulsive_force nw x y k in
        let fx2, fy2 = compute_repulsive_force ne x y k in
        let fx3, fy3 = compute_repulsive_force sw x y k in
        let fx4, fy4 = compute_repulsive_force se x y k in
        (fx1 +. fx2 +. fx3 +. fx4, fy1 +. fy2 +. fy3 +. fy4)

let compute_forces k g = (*computes the forces on the nodes in the graph for the fruchterman reingold algorithm, optimised using the barnes hut approximation*)
  let forces = Hashtbl.create (Hashtbl.length g) in

  (* Initialize displacement vectors for each node *)
  Hashtbl.iter (fun id _ -> Hashtbl.add forces id (0., 0.)) g;

  (* Build quadtree from graph nodes *)
  let quadtree = ref Empty in
  Hashtbl.iter (fun id (_, (x, y)) ->
    quadtree := insert_quadtree !quadtree x y id
  ) g;

  (* Compute repulsive forces using Barnes-Hut *)
  Hashtbl.iter (fun id (_, (x, y)) ->
    let fx, fy = compute_repulsive_force !quadtree x y k in
    Hashtbl.replace forces id (fx, fy)
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

let scale_positions g factor = (*scales the positions of the nodes in the graph*)
  Hashtbl.iter (fun id (adj, (x, y)) ->
    Hashtbl.replace g id (adj, (x *. factor, y *. factor))
  ) g

let cool g temp = (*controls the cooling factor in the fruchterman reingold algorithm*)
  let n = float_of_int (Hashtbl.length g) in
  let cooling_factor =
    if n <= 20. then 0.995
    else if n >= 100. then 0.99995
    else 0.995 +. ((0.99995 -. 0.995) *. ((n -. 20.) /. 80.))
  in
  temp *. cooling_factor
  

let fruchterman_reingold g max_iter= (*implements the fruchterman reingold algorithm*)
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
      Hashtbl.replace g id1 (adj, (newx, newy))
    ) g;
    temp := cool g !temp;
    (* temp := !temp *. 0.99999995; *)
    incr curr_iter
  done;
  Printf.printf "Converged after %d iterations\n" !curr_iter