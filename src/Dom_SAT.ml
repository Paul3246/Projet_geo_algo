open Smtml
open Graph

module Z3 = Solver.Batch(Z3_mappings) (*Z3 solver*)

let get_ordered_nodes (g : Graph.t) : int list = (*returns the nodes in the graph in ascending order*)
  let nodes = Hashtbl.fold (fun id _ acc -> id :: acc) g [] in
  List.sort (fun a b -> compare a b) nodes

let get_domination_constraints (g : Graph.t) : Expr.t * Expr.t array = (*returns the domination constraints for the graph*)
  let ord_nodes = get_ordered_nodes g in
  let vars = Array.of_list (List.map (fun id -> Expr.symbol (Symbol.make Ty.Ty_bool ("x" ^ string_of_int id))) ord_nodes) in
  let node_to_var = Hashtbl.create 100 in
  List.iter2 (fun id expr -> Hashtbl.add node_to_var id expr) ord_nodes (Array.to_list vars);

  let domination_formula =
    List.fold_left (fun acc id ->
      let x_i = Hashtbl.find node_to_var id in
      let nghs = neighbors g id in
      let disj =
        x_i :: List.map (fun j -> Hashtbl.find node_to_var j) nghs
        |> List.fold_left (fun acc xj -> Expr.binop Ty.Ty_bool Or xj acc)
              (Expr.value False)
      in
      Expr.binop Ty.Ty_bool And acc disj
    ) (Expr.value True) ord_nodes
  in
  domination_formula, vars

(*adds cardinality constraints to the solver using Sinz's and Knuth's encoding, for reference see: A comparison of encodings for cardinality constraints in a SAT solver by Ed Wynn*)
let add_card_constraints (solver: Z3.t) (vars : Expr.t array) (r : int) =
  (*vars makes sure we use existing variables to avoid the creation of non significant variables*)
  let n = Array.length vars in
  if r >= n then () else
  let get_s_kj k j = Expr.symbol (Symbol.make Ty.Ty_bool (Printf.sprintf "s_%d_%d" k j)) in

  (* Sequential encoding *)
  for k = 1 to r do
    for j = 1 to n - r - 1 do
      let s_kj = get_s_kj k j in
      let s_kj1 = get_s_kj k (j + 1) in
      Z3.add solver [Expr.binop Ty.Ty_bool Or (Expr.unop Ty.Ty_bool Not s_kj) s_kj1]
    done
  done;

  for k = 0 to r - 1 do
    for j = 0 to n - r - 1 do
      let s_kj = get_s_kj k j in
      let s_k1j = get_s_kj (k+1) j in
      let x_jk = vars.(j + k) in
      let clause = Expr.binop Ty.Ty_bool Or (Expr.unop Ty.Ty_bool Not s_kj)
                      (Expr.binop Ty.Ty_bool Or s_k1j (Expr.unop Ty.Ty_bool Not x_jk))
      in
      Z3.add solver [clause]
    done
  done;

  for j = 0 to n - r - 1 do
    Z3.add solver [get_s_kj 0 j]
  done;

  for j = 0 to n - r - 1 do
    Z3.add solver [Expr.unop Ty.Ty_bool Not (get_s_kj r j)]
  done


let model_to_string model : (string * string) list = (*converts a model to a string for debugging*)
  List.fold_left (fun acc (name, value) ->
    let name_str = Symbol.to_string name in
    let value_str = Value.to_string value in
    (name_str, value_str) :: acc
  ) [] model

let solve_domination (solver: Z3.t) (g : Graph.t) = (*solves the domination problem for the graph using Z3*)
  let cond, _ = get_domination_constraints g in
  Z3.add solver [cond];
  match Z3.check solver [] with
  | `Sat ->
    begin
      let model = match Z3.model solver with
        | Some m -> m
        | None -> failwith "no model"
      in
      Model.get_bindings model |> model_to_string
    end
  | `Unsat -> []
  | `Unknown -> []

let solve_domination_with_card_constraints (solver: Z3.t) (g : Graph.t) (r : int) = (*solves the domination problem with cardinality constraints using Z3*)
  let cond, vars = get_domination_constraints g in
  let r = r-1 in
  Z3.add solver [cond];
  add_card_constraints solver vars  r;
  match Z3.check solver [] with
  | `Sat ->
    begin
      let model = match Z3.model solver with
        | Some m -> m
        | None -> failwith "no model"
      in
      Model.get_bindings model |> model_to_string
    end
  | `Unsat -> []
  | `Unknown -> []

let extract_and_exclude_sol (model : (string * string) list) : Expr.t = (*extracts a solution and returns a SAT formula, this formula is used to exclude the solution from the search space*)
  List.fold_left (fun acc (name, value) ->
    if String.contains name 'x' then
      let x_i = Expr.symbol (Symbol.make Ty.Ty_bool name) in
      let x_i_expr = match value with
        | "true" -> Expr.unop Ty.Ty_bool Not x_i
        | "false" -> x_i
        | _ -> failwith "unexpected value"
      in
      Expr.binop Ty.Ty_bool Or acc x_i_expr
    else
      acc
  ) (Expr.value False) model

let find_smallest_sol sol_hashtbl = (*finds the solution with the least variables set to true among all solutions*)
  let sol_list = Hashtbl.fold (fun _ sol acc -> sol :: acc) sol_hashtbl [] in
  let rec find_smallest_sol' sol_list min_sol min_count =
    match sol_list with
    | [] -> min_sol
    | hd :: tl ->
      let count = List.fold_left (fun acc (_, value) -> if value = "true" then acc + 1 else acc) 0 hd in
      if count < min_count then
        find_smallest_sol' tl hd count
      else
        find_smallest_sol' tl min_sol min_count
  in
  find_smallest_sol' sol_list [] max_int


let solve_MDS_brute_force_with_SAT (g : Graph.t) : (string * string) list = (*solves the minimum dominating set problem with brute force and SAT*)
  let solver = Z3.create () in
  let acc = Hashtbl.create 100 in
  let rec solve count =
    let curr_sol = solve_domination solver g in
    match curr_sol with
    | [] -> acc
    | _ ->
      let exclusion = extract_and_exclude_sol curr_sol in
      Hashtbl.add acc count curr_sol;
      Z3.add solver [exclusion];
      solve (count + 1)
  in
  let sol_list = solve 0 in
  let opt_sol = find_smallest_sol sol_list in
  opt_sol

let get_number_of_true_xi_vars (sol : (string * string) list) : int = (*returns the number of variables set to true in the solution*)
  List.fold_left (fun acc (name, value) ->
    if String.contains name 'x' && value = "true" then
      acc + 1
    else
      acc
  ) 0 sol

let find_sol_less_r (g : Graph.t) (r : int) : (string * string) list = (*finds a solution with less than r variables set to true*)
  let solver = Z3.create () in
  let sol = solve_domination_with_card_constraints solver g r in
  sol

let binary_search_MDS (g : Graph.t) : (string * string) list = (*Solves minimum dominating set using binary search on the size of the solution*)
  let n = Hashtbl.length g in
  let rec search l r best =
    if l > r then
      best
    else
      let m = l + (r - l) / 2 in
      let sol = find_sol_less_r g m in
      if sol = [] then
        search (m + 1) r best
      else
        search l (m - 1) sol
  in
  search 0 n []

  let binary_search_MDS_less_r (g : Graph.t) (r : int) : (string * string) list = (*Solves minimum dominating set using binary search on the size of the solution which is bounded by r*)
    let n = r in
    let rec search l r best =
      if l > r then
        best
      else
        let m = l + (r - l) / 2 in
        let sol = find_sol_less_r g m in
        if sol = [] then
          search (m + 1) r best
        else
          search l (m - 1) sol
    in
    search 0 n []