let median_of_list (lst : float list) (len : int) : float = (*computes the median of a list*)
  match len with
  | 0 -> invalid_arg "median_of_list: empty list"
  | _ when len mod 2 = 1 ->
      List.nth lst (len / 2)
  | _ ->
      let a = List.nth lst (len / 2 - 1) in
      let b = List.nth lst (len / 2) in
      (a +. b) /. 2.0

let absorb_isolated_into_separator g separator_tbl left_tbl right_tbl = (*absorbs isolated nodes into the separator, avoids having isolated nodes in the partitions of the graph*)
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

let geometric_separator_along_direction (dx, dy) (g : Graph.t) = (*computes a geometric separator along a given direction*)
  let nodes =
    Hashtbl.fold (fun id (_, (x, y)) acc ->
      let proj = x *. dx +. y *. dy in
      (id, proj) :: acc
    ) g []
  in
  let sorted = List.sort (fun (_, p1) (_, p2) -> compare p1 p2) nodes in
  let len = List.length sorted in
  let median_proj = median_of_list (List.map snd sorted) len in

  let side = Hashtbl.create len in
  let left_set = Hashtbl.create len in
  let right_set = Hashtbl.create len in

  (* Classify nodes *)
  List.iter (fun (id, proj) ->
    if proj < median_proj then (
      Hashtbl.add side id `Left;
      Hashtbl.add left_set id ()
    ) else (
      Hashtbl.add side id `Right;
      Hashtbl.add right_set id ()
    )
  ) sorted;

  (* Identify separator via cross-edges *)
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

  absorb_isolated_into_separator g separator left_set right_set;

  let to_list tbl = Hashtbl.fold (fun id _ acc -> id :: acc) tbl [] in
  let separator = to_list separator in
  let left = to_list left_set in
  let right = to_list right_set in
  (separator, left, right)

let separator_balance left right = (*computes the balance of a separator*)
  let l = List.length left in
  let r = List.length right in
  float (min l r) /. float (l + r)

let separator_score separator left right = (*computes the score of a separator, based on its balance and size*)
  let balance = separator_balance left right in
  let size_penalty = float (List.length separator + 1) in
  balance /. size_penalty

  let choose_random_separators g = (*chooses random separators and returns the best one, systematically tests the x and y axis and then 10 random directions*)
    let best_score = ref neg_infinity in
    let best_separator = ref [] in
    let best_left = ref [] in
    let best_right = ref [] in
    let x_separator, x_left, x_right = geometric_separator_along_direction (1.0, 0.0) g in
    let x_score = separator_score x_separator x_left x_right in
    if x_score > !best_score then (
      best_score := x_score;
      best_separator := x_separator;
      best_left := x_left;
      best_right := x_right
    );
    let y_separator, y_left, y_right = geometric_separator_along_direction (0.0, 1.0) g in
    let y_score = separator_score y_separator y_left y_right in
    if y_score > !best_score then (
      best_score := y_score;
      best_separator := y_separator;
      best_left := y_left;
      best_right := y_right
    );
    for _ = 1 to 10 do
      let angle = Random.float (2.0 *. Float.pi) in
      let dx = cos angle in
      let dy = sin angle in
      let separator, left, right = geometric_separator_along_direction (dx, dy) g in
      let score = separator_score separator left right in
      if score > !best_score then (
        best_score := score;
        best_separator := separator;
        best_left := left;
        best_right := right
      )
    done;
    (!best_separator, !best_left, !best_right)


(* the following code computes a spectral separator not used in practice, because tested to be less efficient than the geometric separator *)

(* let spectral_separator g = (*computes a spectral separator, using the Fiedler vector of the Laplacian matrix of the graph*)
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