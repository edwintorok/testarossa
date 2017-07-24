open Logs

let n = 2

type states = Active | CleanShutdown

module NodeId = struct
  type t = int

  let lowest = 1

  let pp = Fmt.int
  let compare (a:t) (b:t) = Pervasives.compare a b
end

module NodeSet = Set.Make(NodeId)

type node = {
  id: NodeId.t;
  state: states option; (* corosync state *)
  node_set : NodeSet.t;
}
let node_compare a b =
  let d = NodeId.compare a.id b.id in
  if d = 0 then
    let d = Pervasives.compare a.state b.state in
    if d = 0 then
      NodeSet.compare a.node_set b.node_set
    else d
  else d

(* clean shutdown: only one at a time, confirmed by quorum *)

module NodeMap = Map.Make(NodeId)

module LinkSet = Set.Make(struct type t = NodeId.t * NodeId.t let compare = compare end)


let can_reach links a b =
  LinkSet.mem (a, b) links

let can_symmetric_reach links a b =
  can_reach links a b &&
  can_reach links b a

let is_quorate links node =
  if node.state <> Some Active then false
  else
  let n = NodeSet.cardinal node.node_set in
  let seen =
    node.node_set |>
    NodeSet.filter (can_symmetric_reach links node.id) |>
    NodeSet.cardinal in
  let result =
  seen > n / 2 ||
  (seen == n /2 && can_symmetric_reach links NodeId.lowest node.id) in
  Logs.debug (fun m -> m "Node %d sees %d/%d nodes, quorate: %b"
                 node.id seen n result);
  result

let pp_node =
  Fmt.(using (fun node -> node.id) int)

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
             These nodes cannot reach node %a: %a"
           pp_node
           hd
           Fmt.(using fst NodeId.pp |> list ~sep:(always ",") |> using NodeMap.bindings) tl
        );
      true
    end

let join ~clusternode b nodes =
  let clusternode = NodeMap.find clusternode nodes in
  assert (clusternode.state == Some Active);

  let activate_node id nodes =
    let node = NodeMap.find b nodes in
    assert (node.state == None);
    NodeMap.add b { node with state = Some Active } nodes
  in

  let nodes_to_update = clusternode.node_set in
  nodes |>
  NodeSet.fold (fun id map ->
      let node = NodeMap.find id map in
      let node_set = NodeSet.add id node.node_set in
      let node' = { node with node_set } in 
      NodeMap.add id node' map
    ) nodes_to_update |>
  activate_node b

let remove ~clusternode b nodes =
  let clusternode = NodeMap.find clusternode nodes in
  assert (clusternode.state == Some Active);
  let deactivate_node id nodes =
    let node = NodeMap.find b nodes in
    assert (node.state <> None);
    NodeMap.add b { node with state = None } nodes
  in

  let nodes_to_update = clusternode.node_set in
  nodes |>
  NodeSet.fold (fun id map ->
      let node = NodeMap.find id map in
      let node_set = NodeSet.remove id node.node_set in
      let node' = { node with node_set } in 
      NodeMap.add id node' map
    ) nodes_to_update |>
  deactivate_node b


module State = struct
  type t = LinkSet.t * node NodeMap.t
  let compare (l1,n1) (l2,n2) =
    let d = LinkSet.compare l1 l2 in
    if d = 0 then NodeMap.compare node_compare n1 n2
    else d

  let initial =
    let links : LinkSet.t =
      let a = Array.init n (fun i ->
          let a = Array.init n (fun j -> (i,j)) in
          Array.fold_right LinkSet.add a LinkSet.empty
        ) in
      Array.fold_left LinkSet.union LinkSet.empty a
    in
    let nodes : node NodeMap.t =
      let node_ids = Array.init n (fun i -> i+1) in
      let node_set = Array.fold_left (fun acc id ->
          NodeSet.add id acc
        ) NodeSet.empty node_ids in
      Array.fold_left (fun acc id ->
          NodeMap.add id 
          {
            id;
            state = if id == NodeId.lowest then Some Active else None;
            node_set = node_set;
          } acc
        ) NodeMap.empty node_ids
    in
    links, nodes

  let pp ppf (links, nodes) =
    Format.fprintf ppf "%d links; %d nodes"
      (LinkSet.cardinal links)
      (NodeMap.cardinal nodes)
end

module StateSet = Set.Make(State)

let rec explore seen state =
  Logs.debug (fun m -> m "State: %a" State.pp state);
  let (links, nodes) = state in
  let n = NodeMap.cardinal nodes in
  let go state' =
    if StateSet.mem state' !seen then ()
    else (* explore only new states *)
    seen := StateSet.add state' !seen;
    if not (check_for_split_brain state') then
      explore seen state'
  in
  for i = 1 to n do
    let node = NodeMap.find i nodes in
    if node.state == Some Active then
      for j = i+1 to n do
        let node' = NodeMap.find j nodes in
        if node'.state == None then
          go (links, join ~clusternode:i j nodes)
        else
          go (links, remove ~clusternode:i j nodes)
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

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  explore (ref StateSet.empty) State.initial
