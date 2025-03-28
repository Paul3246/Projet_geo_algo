let median_of_list (lst : float list) (len : int) : float =
  match len with
  | 0 -> invalid_arg "median_of_list: empty list"
  | _ when len mod 2 = 1 ->
      List.nth lst (len / 2)
  | _ ->
      let a = List.nth lst (len / 2 - 1) in
      let b = List.nth lst (len / 2) in
      (a +. b) /. 2.0

let absorb_isolated_into_separator g separator_tbl left_tbl right_tbl =
  let to_promote = ref [] in
  Hashtbl.iter (fun id (adj, _) ->
    let is_sep = Hashtbl.mem separator_tbl id in
    if not is_sep then (
      let all_neighbors_in_sep =
        Hashtbl.fold (fun nbr _ acc ->
          acc && Hashtbl.mem separator_tbl nbr
        ) adj true
      in
      if all_neighbors_in_sep then
        to_promote := id :: !to_promote
    )
  ) g;
  (* Move those nodes into the separator *)
  List.iter (fun id ->
    Hashtbl.replace separator_tbl id ();
    Hashtbl.remove left_tbl id;
    Hashtbl.remove right_tbl id
  ) !to_promote

let geometric_separator_x_coord g =
  let nodes = Hashtbl.fold (fun id (_, (x, _)) acc -> (id, x) :: acc) g [] in
  let sorted = List.sort (fun (_, x1) (_, x2) -> compare x1 x2) nodes in
  let len = List.length sorted in
  let median_x = median_of_list (List.map snd sorted) len in

  let side = Hashtbl.create len in
  let left_set = Hashtbl.create len in
  let right_set = Hashtbl.create len in

  (* Classify nodes *)
  List.iter (fun (id, x) ->
    if x < median_x then (
      Hashtbl.add side id `Left;
      Hashtbl.add left_set id ()
    ) else (
      Hashtbl.add side id `Right;
      Hashtbl.add right_set id ()
    )
  ) sorted;

  (* Identify separator by detecting cross-edges *)
  let separator = Hashtbl.create len in
  Hashtbl.iter (fun u (adj, _) ->
    Hashtbl.iter (fun v _ ->
      match Hashtbl.find_opt side u, Hashtbl.find_opt side v with
      | Some su, Some sv when su <> sv ->
          Hashtbl.replace separator u ();
          Hashtbl.replace separator v ();
          Hashtbl.remove left_set u;
          Hashtbl.remove right_set u;
          Hashtbl.remove left_set v;
          Hashtbl.remove right_set v;
      | _ -> ()
    ) adj
  ) g;
  
  let to_list tbl = Hashtbl.fold (fun id _ acc -> id :: acc) tbl [] in
  absorb_isolated_into_separator g separator left_set right_set;
  let separator = to_list separator in
  let left = to_list left_set in
  let right = to_list right_set in
  (separator, left, right)

let geometric_separator_y_coord g =
  let nodes = Hashtbl.fold (fun id (_, (_, y)) acc -> (id, y) :: acc) g [] in
  let sorted = List.sort (fun (_, y1) (_, y2) -> compare y1 y2) nodes in
  let len = List.length sorted in
  let median_y = median_of_list (List.map snd sorted) len
  in

  let side = Hashtbl.create len in
  let top_set = Hashtbl.create len in
  let bottom_set = Hashtbl.create len in

  (* Classify nodes *)
  List.iter (fun (id, y) ->
    if y < median_y then (
      Hashtbl.add side id `Top;
      Hashtbl.add top_set id ()
    ) else (
      Hashtbl.add side id `Bottom;
      Hashtbl.add bottom_set id ()
    )
  ) sorted;

  (* Identify separator by detecting cross-edges *)
  let separator = Hashtbl.create len in
  Hashtbl.iter (fun u (adj, _) ->
    Hashtbl.iter (fun v _ ->
      match Hashtbl.find_opt side u, Hashtbl.find_opt side v with
      | Some su, Some sv when su <> sv ->
          Hashtbl.replace separator u ();
          Hashtbl.replace separator v ();
          Hashtbl.remove top_set u;
          Hashtbl.remove bottom_set u;
          Hashtbl.remove top_set v;
          Hashtbl.remove bottom_set v;
      | _ -> ()
    ) adj
  ) g;

  let to_list tbl = Hashtbl.fold (fun id _ acc -> id :: acc) tbl [] in
  absorb_isolated_into_separator g separator top_set bottom_set;
  let separator = to_list separator in
  let top = to_list top_set in
  let bottom = to_list bottom_set in
  (separator, top, bottom)

let separator_balance left right =
  let l = List.length left in
  let r = List.length right in
  float (min l r) /. float (l + r)

let separator_score separator left right =
  let balance = separator_balance left right in
  let size_penalty = float (List.length separator + 1) in
  balance /. size_penalty

let choose_separator g =
  let separator_x, left_x, right_x = geometric_separator_x_coord g in
  let separator_y, left_y, right_y = geometric_separator_y_coord g in
  let score_x = separator_score separator_x left_x right_x in
  let score_y = separator_score separator_y left_y right_y in
  if score_x < score_y then (separator_x, left_x, right_x)
  else (separator_y, left_y, right_y)

(* let spectral_separator g =
  let lap = laplacian_matrix g in
  let fiedler = fiedler_vector lap in
  let nodes =
    Hashtbl.fold (fun id _ acc ->
      let value = fiedler.(id - 1) in
      (id, value) :: acc
    ) g []
  in
  let sorted = List.sort (fun (_, v1) (_, v2) -> compare v1 v2) nodes in
  let len = List.length sorted in
  let median =
    match List.nth_opt sorted (len / 2) with
    | Some (_, v) -> v
    | None -> 0.0
  in
  let delta = 0.02 in  (* tolerance around median to define separator *)
  let separator, left, right =
    List.fold_left (fun (s, l, r) (id, v) ->
      if abs_float (v -. median) < delta then (id :: s, l, r)
      else if v < median then (s, id :: l, r)
      else (s, l, id :: r)
    ) ([], [], []) sorted
  in
  (separator, left, right) *)