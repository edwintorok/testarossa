open Logs

let n = 3

type states = Active | CleanShutdown

module NodeId = struct
  type t = int

  let lowest = 1

  let pp = Fmt.int
  let compare (a:t) (b:t) = Pervasives.compare a b
end

module NodeMap = Map.Make(NodeId)

type node = {
  id: NodeId.t;
  state: states option; (* corosync state *)
  node_set : int NodeMap.t; (* node -> votes *)
}

let int_compare (a:int) (b:int) = Pervasives.compare a b

let node_compare a b =
  let d = NodeId.compare a.id b.id in
  if d = 0 then
    let d = Pervasives.compare a.state b.state in
    if d = 0 then
      NodeMap.compare int_compare a.node_set b.node_set
    else d
  else d

let node_pp ppf node =
  Format.fprintf ppf "[id: %d; state: _; node_set: %a]"
    node.id
    Fmt.(list ~sep:(always ",") (pair ~sep:(always "->") NodeId.pp int))
    (NodeMap.bindings node.node_set)


(* clean shutdown: only one at a time, confirmed by quorum *)

module LinkSet = Set.Make(struct
    type t = NodeId.t * NodeId.t
    let compare = compare
end)


let can_reach links a b =
  a == b ||
  LinkSet.mem (a, b) links

let can_symmetric_reach links a b =
  can_reach links a b &&
  can_reach links b a

let sum_votes nodes =
  NodeMap.fold (fun _ -> (+)) nodes 0

let is_quorum i n =
  i >= n/2 + 1

let is_tie i n =
  (* beware integer division if using n/2 *)
  i*2 == n

let is_quorate links node =
  if node.state <> Some Active then false
  else
  let all_votes = sum_votes node.node_set in
  let seen =
    node.node_set |>
    NodeMap.filter (fun n _ -> can_symmetric_reach links node.id n) |>
    sum_votes
  in
  let result =
    is_quorum seen all_votes ||
    (is_tie seen all_votes
     && can_symmetric_reach links NodeId.lowest node.id) in
  Logs.debug (fun m -> m "Node %d sees %d/%d nodes, quorate: %b"
                 node.id seen n result);
  result

let pp_node =
  Fmt.(using (fun node -> node.id) int)

module State = struct
  type t = LinkSet.t * node NodeMap.t
  let compare (l1,n1) (l2,n2) =
    let d = LinkSet.compare l1 l2 in
    if d = 0 then NodeMap.compare node_compare n1 n2
    else d

  let initial =
    let links : LinkSet.t =
      let a = Array.init n (fun i ->
          let a = Array.init n (fun j -> (i+1,j+1)) in
          Array.fold_right LinkSet.add a LinkSet.empty |>
          LinkSet.remove (i+1, i+1)
        ) in
      Array.fold_left LinkSet.union LinkSet.empty a
    in
    let nodes : node NodeMap.t =
      let node_ids = Array.init n (fun i -> i+1) in
      Array.fold_left (fun acc id ->
          NodeMap.add id 
          {
            id;
            state = if id == NodeId.lowest then Some Active else None;
            node_set = NodeMap.singleton id 1;
          } acc
        ) NodeMap.empty node_ids
    in
    links, nodes

  let pp_links =
    Fmt.(using LinkSet.elements (pair ~sep:(always "-") NodeId.pp NodeId.pp |>
                                 list ~sep:(always " ")))

  let pp ppf (links, nodes) =
    Format.fprintf ppf "%d links: %a; %a"
      (LinkSet.cardinal links)
      pp_links links
      Fmt.(list (Dump.pair NodeId.pp node_pp))
      (NodeMap.bindings nodes)
end

let check_for_split_brain (links, nodes) =
  let quorate_nodes = NodeMap.filter (fun _ n -> is_quorate links n) nodes in
  if NodeMap.cardinal quorate_nodes <=1 then false
  else
    let id, hd = NodeMap.min_binding quorate_nodes in
    let tl = NodeMap.remove id quorate_nodes in
    let _, partitioned = NodeMap.partition (fun _ node ->
        can_symmetric_reach links hd.id node.id) tl in
    if NodeMap.is_empty partitioned then false
    else begin
      Logs.err (fun m ->
          m "Nodes belonging to different partitions claiming to be quorate!\n\
             State: %a\n\
             These nodes cannot reach node %a: %a"
           State.pp (links, nodes)
           pp_node
           hd
           Fmt.(using fst NodeId.pp |> list ~sep:(always ",") |> using NodeMap.bindings) tl
        );
      true
    end

let join ~clusternode b nodes =
  let clusternode = NodeMap.find clusternode nodes in
  assert (clusternode.state == Some Active);
  let node_set = NodeMap.add b 1 clusternode.node_set in

  let activate_node id nodes =
    let node = NodeMap.find b nodes in
    assert (node.state == None);
    NodeMap.add b { node with state = Some Active; node_set } nodes
  in

  let nodes_to_update = clusternode.node_set in
  nodes |>
  NodeMap.fold (fun id votes map ->
      let node = NodeMap.find id map in
      Logs.debug (fun m -> m "added %d, new set: %a"
                     id
                     Fmt.(list ~sep:(always ",") (pair NodeId.pp int))
                     (NodeMap.bindings node.node_set)
                 );
      let node' = { node with node_set } in 
      NodeMap.add id node' map
    ) nodes_to_update |>
  activate_node b

let remove ~links ~clusternode b nodes =
  let clusternode = NodeMap.find clusternode nodes in
  assert (clusternode.state == Some Active);
  (* force remove: no quorum checks *)
  let deactivate_node id nodes =
    (* force-remove: no deactivation *)
(*    let node = NodeMap.find b nodes in
    assert (node.state <> None);
      NodeMap.add b { node with state = None } nodes*)
    nodes
  in

  let nodes_to_update = clusternode.node_set in
  nodes |>
  NodeMap.fold (fun id _votes map ->
      if can_reach links clusternode.id id then
        let node = NodeMap.find id map in
        let node_set = NodeMap.remove b node.node_set in
        let node' = { node with node_set } in 
        NodeMap.add id node' map
      else map
    ) nodes_to_update |>
  deactivate_node b


module StateSet = Set.Make(State)

let cut i j links =
  links |>
  LinkSet.remove (i, j) |>
  LinkSet.remove (j, i) 

let reconnect i j links =
  links |>
  LinkSet.add (i, j) |>
  LinkSet.add (j, i)

let rec explore seen state =
  Logs.debug (fun m -> m "State: %a" State.pp state);
  let (links, nodes) = state in
  let n = NodeMap.cardinal nodes in
  let go state' =
    if not (StateSet.mem state' !seen) then begin
      (* explore only new states *)
      seen := StateSet.add state' !seen;
      if not (check_for_split_brain state') then
        explore seen state'
    end
  in
  for i = 1 to n do
    let node = NodeMap.find i nodes in
    if node.state == Some Active then
      for j = i+1 to n do
        let node' = NodeMap.find j nodes in
        if node'.state == None then
          go (links, join ~clusternode:i j nodes)
        else
          go (links, remove ~links ~clusternode:i j nodes)
      done
  done;

  for i = 1 to n do
    for j = i+1 to n do
      if can_reach links i j then
        go (cut i j links, nodes)
      else
        go (reconnect i j links, nodes)
    done
  done
  (* TODO: create cuts and partitions by removing links *)
        
(*open Cmdliner

let n_arg =
  Arg.(value & int & info ["n"] ~docv:"NODES")

let main_t = Term.(pure main $ n_arg)

let info =
  let doc = "Checks quorum/rejoin safety" in
  Term.info "quorum/rejoin tester" ~version:"0.1" ~doc

let () =
  Term.exit @@ Term.eval (main_t, info)*)

let do_split_brain () =
  let links, nodes = State.initial in
  let nodes = nodes |>
              join ~clusternode:1 2 |>
              join ~clusternode:1 3
  in
  let links = links |>
              cut 1 2 |>
              cut 1 3 |> 
              cut 2 4 |>
              cut 2 5 |>
              cut 3 4 |>
              cut 3 5
  in
  let nodes = nodes |>
              remove ~links ~clusternode:1 2 |>
              remove ~links ~clusternode:1 3
  in
  let nodes = nodes |>
              join ~clusternode:1 4 |>
              join ~clusternode:1 5
  in
  let links = links |>
              cut 1 4 |>
              cut 1 5
  in
  let state = (links, nodes) in
  Format.printf "state: %a\n%!" State.pp  state;
  assert (check_for_split_brain state)

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  explore (ref StateSet.empty) State.initial
