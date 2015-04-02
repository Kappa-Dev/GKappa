(**
 * gkapa.ml 
 * GKappa
 * Jérôme Feret, projet Antique, INRIA Paris-Rocquencourt
 * 
 * Creation: March, the 28th of 2015
 * Last modification: April, the 2nd of 2015
 * * 
 *  
 * Copyright 2015 Institut National de Recherche en Informatique  * et en Automatique.  All rights reserved.  
 * This file is distributed under the terms of the 
 * GNU Library General Public License *)


(* GKappa is a OCAML library to help making slides in Kappa *)
(* More applications are coming soon (hopefully) *)

open Data_structures 
module type GKappa =
  sig
    type config = 
      { 
	show_agent_names: bool;
	show_site_names: bool;
	show_state_names: bool;
	show_free_symbols: bool;
	color_agents: bool;
	color_sites: bool;
	color_states: bool ;
	site_radius: float ;
	agent_colors: string list ;
	site_colors: string list ;
	state_colors: string list ;
	pi : float ;
	free_width : float ;
	free_height : float ;
	bound_height : float ; 
	rule_length: float ;
	rule_width: int;
	edge_label_font: int;
      }
    type tag = string 
    type directive =
      Fontsize of int 
    | Tag of tag * int  
    | Radius of float 
    | Width of float 
    | Height of float 
    | Direction of float  
    | Shape of string 
    | Set_scale of float 
    | Scale of float 
    | Color of string 
    | FillColor of string 
    | Comment of string 

    type id 
    type agent_type
    type site_type
    type internal_state_type
    type node 
    type state_type = Free of directive list 
		 | Bound of directive list
		 | Internal of internal_state_type * directive list
   
    type agent 
    type site
    type state 
    type remanent_state 
    type graph_vars = (agent * (site * state list ) list ) list 
    type graph = 
      (agent_type *float*float*directive list* 
	 (site_type * directive list* 
	  state_type list) list) list
    val init: config -> agent_type * remanent_state 
    val add_agent_type: string -> directive list -> remanent_state -> agent_type * remanent_state 
    val add_site_type: agent_type -> string -> directive list -> remanent_state -> site_type * remanent_state 
    val add_internal_state_type: site_type -> string -> directive list -> remanent_state -> internal_state_type * remanent_state 

    val add_in_graph: remanent_state -> graph -> remanent_state * graph_vars 
    val add_agent: agent_type -> float -> float -> directive list -> remanent_state -> agent * remanent_state 
    val add_site: agent -> site_type  -> directive list -> remanent_state -> site * remanent_state 
    val add_internal_state: site -> internal_state_type -> directive list -> remanent_state -> state * remanent_state 
    val add_free: site -> directive list -> remanent_state -> state * remanent_state 
    val add_bound: site -> directive list -> remanent_state -> state * remanent_state 

    val add_free_list: (site * directive list) list -> remanent_state -> state list * remanent_state

    val s:float
    val n:float
    val ne:float
    val nw:float
    val w:float
    val e:float
    val se:float
    val sw:float 
    
    val add_strong_flow_and_link: site -> site -> remanent_state -> remanent_state 
    val add_flow_and_link_list : (site*site) list -> remanent_state -> remanent_state
    val add_flow_list: (site*site) list -> remanent_state -> remanent_state 
    val add_link_list: (site*site) list -> remanent_state -> remanent_state 
    val map_id: (id -> id) -> remanent_state -> 
      (agent->agent)*(site->site)*(state->state)*remanent_state 

    val dump: string -> (tag*int) list -> remanent_state -> unit
    val rotate: float ->float-> float->remanent_state ->remanent_state
    val translate: float -> float -> remanent_state -> remanent_state
    val sym: float -> float ->remanent_state -> remanent_state
    val fuse: remanent_state -> remanent_state -> remanent_state
    val horizontal_swap: remanent_state -> remanent_state 
    val vertical_swap: remanent_state -> remanent_state 
    val disjoint_union: remanent_state -> remanent_state -> 
      (agent -> agent)*(site -> site)*(state->state)*(agent->agent)*(site->site)*(state->state)*remanent_state 
    val disjoint_union_with_match: remanent_state -> remanent_state -> 
      (agent -> agent)*(site -> site)*(state->state)*(agent->agent)*(site->site)*(state->state)*remanent_state 
    val add_match: (agent*agent) list -> remanent_state -> remanent_state 
    val add_proj: (agent*agent) list -> remanent_state -> remanent_state 
    val add_emb: (agent*agent) list -> remanent_state -> remanent_state
    val tag_all_nodes: tag -> int -> remanent_state -> remanent_state 
    val move_remanent_right_to: float -> remanent_state -> remanent_state -> remanent_state
    val move_remanent_left_to: float -> remanent_state -> remanent_state -> remanent_state 
    val move_remanent_above: float -> remanent_state -> remanent_state -> remanent_state 
    val move_remanent_bellow: float -> remanent_state -> remanent_state -> remanent_state 
    val add_rule: float -> float -> directive list -> remanent_state -> remanent_state
    val corners: remanent_state -> (float * float * float * float) option 
    val cross: remanent_state -> remanent_state 
end

module GKappa = 
  (struct 

    type tag = string 
    type directive = 
      Fontsize of int 
    | Tag of tag * int  
    | Radius of float 
    | Width of float 
    | Height of float 
    | Direction of float  
    | Shape of string 
    | Set_scale of float 
    | Scale of float 
    | Color of string 
    | FillColor of string 
    | Comment of string 
    type id = int 
    type agent_type = id 
    type site_type = agent_type * id 
    type internal_state_type = agent_type * id * id 
    type agent = id * id 
    type site = id * id * id * (id * id)
    type state = id * id * id 
    type state_type = Free of directive list 
		 | Bound of directive list
		 | Internal of internal_state_type * directive list
   
    type graph_vars = (agent * (site * state list ) list ) list 
    type graph = 
      (agent_type *float*float*directive list* 
	 (site_type * directive list* 
	  state_type list) list) list

 
    let init_id = 0 
    let succ x = x+1 

    module TagMap = Map.Make (struct type t = tag let compare = compare end)
    module IntSet = Set.Make (struct type t = int let compare = compare end)
    module IdMap = Data_structures.Make (struct type t = id let compare = compare end)
    module Id2Map = Data_structures.Make (struct type t = id * id let compare = compare end)
    module StringMap = Map.Make (struct type t = string let compare = compare end)
    module String2Map = Map.Make (struct type t = string * string let compare = compare end) 

    type config = 
      { 
	show_agent_names: bool;
	show_site_names: bool;
	show_state_names: bool;
	show_free_symbols: bool;
	color_agents: bool;
	color_sites: bool;
	color_states: bool ;
	site_radius: float ;
	agent_colors: string list ;
	site_colors: string list ;
	state_colors: string list ;
	pi : float ;
	free_width : float ;
	free_height : float ;
	bound_height : float ;
	rule_length : float ;
	rule_width: int;
	edge_label_font:int;
      }
	
    type intset = IntSet.t 
    type node = 
      { 
	name: string ; 
	label: string ; 
	tags: intset TagMap.t ;
	width: float ;
	height: float ;
	fontsize: int ; 
	shape: string ; 
	fillcolor:string ;
	color:string;
	abscisse: float ;
	ordinate: float 
      }
	
	
	
 
    let n = 0. 
    let ne = 45. 
    let e = 90.
    let se = 135. 
    let s = 180.
    let sw = 225. 
    let w = 270. 
    let nw = 315. 
      
    let dummy_node = 
      { 
	name = "" ;
	label = "" ;
	tags = TagMap.empty; 
	width = 0.;
	height = 0.;
	fontsize = 0; 
	fillcolor = "white";
	color = "black";
	abscisse = 0.;
	ordinate = 0.;
	shape = "ellipse";
      }
	
    let dummy_sig_agent_node = 
      { 
	dummy_node with
	  width = 2.;
	  height = 1.;
	  fontsize = 20; 
      }
	
    let dummy_sig_site_node = 
      { 
	dummy_node with 
	  width = 0.4;
	  height = 0.4;
	  fontsize = 14; 
      }
	
    let dummy_sig_state_node = 
      { 
	dummy_node with 
	  width = 0.1;
	  height = 0.1;
	  fontsize = 10; 
      }
	
	
    type sig_state = 
      { 
	sig_state_node: node;
	sig_state_direction: float ;
	sig_state_scale_factor: float ;
      }
	
	
	
    let init_sig_state = 
      {
	sig_state_node = dummy_sig_state_node ;
	sig_state_direction = 0. ;
	sig_state_scale_factor = 1.;
      }
	
    type sig_site = 
      { 
	sig_site_node: node ;
	sig_site_states: sig_state IdMap.map ; 
	sig_site_n_states: int ;  
	sig_site_direction: float ;
	sig_site_scale_factor: float ;
      }
	
    let init_sig_site = 
      { sig_site_node = dummy_sig_site_node ;
	sig_site_states = IdMap.empty_map ; 
	sig_site_n_states = 0; 
	sig_site_direction = 0.; 
	sig_site_scale_factor = 1.05}
	
    type sig_agent = 
      { 
	sig_agent_node: node;
	sig_agent_sites: sig_site IdMap.map ; 
	sig_agent_n_sites: int 
      }
	
    let init_sig_agent = 
      { sig_agent_node = dummy_sig_agent_node;
	sig_agent_sites = IdMap.empty_map; 
	sig_agent_n_sites = 0}
	
    type 'a kind = Agent of 'a | Site of 'a | State of 'a 
	
    type edge =
      { 
	node1: node;
	node2: node;
	forward: bool;
	backward: bool; 
	priority: int;
	color: string ;
	edges_tag: intset TagMap.t ;
	width: int;
	style: string;
	comment: string ;
      }
	
    let link = 
      {
	style = "";
	width = 2;
	comment = ""     ;
	node1 = dummy_node ;
	node2 = dummy_node ; 
	forward = false ;
	backward = false ;
	color = "black" ;
	priority = 0 ;
	edges_tag = TagMap.empty 
      }

    let dummy_match = 
      {
       	link with 
	  width=2;
	  style = "dashed";
	color = "cyan" ;
	priority = 0 ;}
    let dummy_proj = 
      { dummy_match with forward = true}
	
       
	
    let rule = 
      { 
	link with forward = true}

    let weak_flow = 
      { link 
	with 
	  priority = 1;
	  forward = true;
	  color = "cyan";
	  edges_tag = TagMap.add "flow" (IntSet.add 1 (IntSet.add 2 IntSet.empty)) TagMap.empty} 
	
	
    let flow = 
      { weak_flow with 
	priority = 2;
	color = "red" ;
	edges_tag = TagMap.add "flow" (IntSet.add 1 IntSet.empty) TagMap.empty}
	
    let strong_flow = 
      {flow with edges_tag = TagMap.add "flow" (IntSet.add 2 IntSet.empty) TagMap.empty}
	
    type state_node = Internal_node of node | Free_nodes of node*node*node | Bound_node of node 
	
    type site_node = 
      { 
	site_node: node;
	site_states: state_node IdMap.map ;
	site_direction: float ;
      }
	
    type agent_node = 
      { 
	agent_type: agent_type ;
	agent_node: node ;
	agent_sites: site_node IdMap.map
      }
	
    let dummy_agent = 
      { 
	agent_type = 0 ;
	agent_node = dummy_sig_agent_node ;
	agent_sites = IdMap.empty_map
      }
    let dummy_site = 
      {
	site_node= dummy_sig_site_node; 
	site_states= IdMap.empty_map;
	site_direction = 0.;
      }
	
    let dummy_state = dummy_sig_state_node
      
    type remanent_state = 
      {
	config: config ;
	nodes: node IdMap.map;
	nnodes: id;
	agent_types: sig_agent IdMap.map ;
	agents: agent_node IdMap.map ;
	edge_list: edge list Id2Map.map ;
      }
	
	
    let dummy_agent_node = 
      { 
	dummy_node with 
	width = 2.;
	height = 1.;
	fontsize = 30; 
      }
      
    let dummy_site_node = 
      { 
	dummy_node with 
	width = 2.;
	height = 1.;
	fontsize = 14; 
      }
	
    let dummy_state_node = 
      { 
	dummy_node with 
	width = 0.4;
	height = 0.4;
	fontsize = 10; 
      }

    let init config = 
      { 
	config = config ;
	nodes = IdMap.empty_map ;
	nnodes = init_id;
	agent_types = IdMap.empty_map ; 
	agents = IdMap.empty_map ;
	edge_list = Id2Map.empty_map ;
      }
	
    let n_modulo list n = 
      let rec aux n l = 
	match l 
	with [] -> aux n list 
	| t::q -> 
	  if n=1 then t else aux (n-1) q 
      in 
      if n<1 || list = [] then 
	(let _ = Printf.fprintf stderr "403\n" in 
	 raise Exit)
      else aux n list 
	
    let angle_of_index i = 
      let all_angles = [45.;135.;225.;315.] in 
      let update l = 
	match l 
	with [] | [_]-> l,l
	| init::_ -> 
	  let rec aux list (res,res2) = 
	    match list 
	    with a::b::c -> 
	      let x = (a+.b)/.2. in 
	      aux (b::c) (x::a::res,x::res2)
	| [b] -> 
	  let x = (init+.b)/.2. in 
	  List.rev (b::x::res),List.rev (x::res2 )
	| [] -> (Printf.fprintf stdout "418\n";raise Exit)
	  in 
	  aux l ([],[])
      in 
      let current_angles = all_angles in 
      let rec aux k all current = 
	match current 
	with []  -> 
	  let all,current = update all in 
	  aux k all current
	| t::q -> 
	  if k=1 then t
	  else aux (k-1) all q 
      in 
      aux i all_angles current_angles
	
    let add_agent_type name attributes remanent = 
      let agent_id = remanent.nnodes + 1 in 
      let remanent = {remanent with nnodes = agent_id } in 
      let agent_sig = 
	{ init_sig_agent 
	  with 
	    sig_agent_node 	= 
	    {
	      init_sig_agent.sig_agent_node 
	     with 
	       label = name;
	       fillcolor = n_modulo (remanent.config.agent_colors) agent_id }}
      in 
      let node = agent_sig.sig_agent_node in 
      let rec parse_attributes att node =
	match 
	  att
	with 
	| [] -> node 
	| t::q -> 
	  parse_attributes q 
	    begin
	      match 
		t 
	      with 
	      | Comment _ -> node 
	      | FillColor s -> {node with fillcolor = s}
	      | Color s -> {node with color = s}
	      | Fontsize i -> {node with fontsize = i}
	      | Tag (_,_) -> let _ = 
				Printf.fprintf stderr "Warning: Tags are useless in agent types declaration\n" 
			      in node
	      | Width f -> {node with width = f}
	      | Height f -> {node with height = f}
	      | Direction f -> 
		let _ = 
		  Printf.fprintf stderr "Warning: Directions are useless in agent types declaration\n" 
		in node
	      | Shape s -> {node with shape = s}
	      | Radius f -> parse_attributes [Width f;Height f] node 
	      | Scale _ | Set_scale _ ->  
		let _ = 
		  Printf.fprintf stderr "Warning: Directions are useless in agent types declaration\n" 
		in node
	    end
      in 
      let node = parse_attributes attributes node in 
      agent_id,
      {remanent with 
	agent_types  = snd (IdMap.add_map () () agent_id {agent_sig with sig_agent_node = node}  remanent.agent_types)}
	
    let add_site_type agent_id name attributes (remanent:remanent_state) = 
      let agent_type = snd (IdMap.find_map () ()  agent_id remanent.agent_types) in 
      let site_id = agent_type.sig_agent_n_sites + 1 in 
      let agent_type = { agent_type with sig_agent_n_sites = site_id} in 
      let color = n_modulo (remanent.config.site_colors) site_id in 
      let angle = angle_of_index site_id in 
      let site_sig = 
	{ init_sig_site 
	  with 
	    sig_site_node = 
	    { init_sig_site.sig_site_node with fillcolor = color ; label = name};
	    sig_site_direction = angle }
      in 
      let rec parse_attributes att site_sig =
	match 
	  att
	with 
	| [] -> site_sig 
	| t::q -> 
	  parse_attributes q 
	    begin
	      match 
		t 
	      with 
	      | Comment _ -> site_sig 
	      | FillColor s -> {site_sig with sig_site_node = {site_sig.sig_site_node  with fillcolor = s}}
	      | Color s -> {site_sig with sig_site_node = {site_sig.sig_site_node with color = s}}
	      | Fontsize i -> {site_sig with sig_site_node = {site_sig.sig_site_node with fontsize = i}}
	      | Tag (_,_) -> let _ = 
				Printf.fprintf stderr "Warning: Tags are useless in site types declaration\n" 
			      in site_sig
	      | Width f -> {site_sig with sig_site_node = {site_sig.sig_site_node with width = f}}
	      | Height f -> {site_sig with sig_site_node = {site_sig.sig_site_node with height = f}}
	      | Direction f -> 
	        {site_sig with sig_site_direction = f}
	      | Shape s -> {site_sig with sig_site_node = {site_sig.sig_site_node with shape = s}}
	      | Radius f -> parse_attributes [Width f;Height f] site_sig
	      | Set_scale f -> 
		{ site_sig with sig_site_scale_factor = f}
	      | Scale f -> parse_attributes [Set_scale (f*.site_sig.sig_site_scale_factor)] site_sig 
	    end
      in 
      let site_sig = parse_attributes attributes site_sig in 
      let agent_type = 
	{agent_type 
	 with 
	   sig_agent_sites = 
	    snd (IdMap.add_map () ()  site_id site_sig agent_type.sig_agent_sites)}
      in 
      (agent_id,site_id),
      {remanent with 
	agent_types = snd (IdMap.add_map () ()  agent_id agent_type remanent.agent_types)}
	
    let add_internal_state_type  (agent_id,site_id) state attributes remanent = 
      let agent_type = snd (IdMap.find_map () ()  agent_id remanent.agent_types) in 
      let site = snd (IdMap.find_map () ()  site_id agent_type.sig_agent_sites) in 
      let state_id = site.sig_site_n_states +1 in 
      let site = { site with sig_site_n_states = state_id} in 
      let state_sig = 
	{ init_sig_state with 
	  sig_state_node = 
	    {init_sig_state.sig_state_node with fillcolor = 
		n_modulo 
		  (remanent.config.state_colors) 
		  state_id}  ;
	  sig_state_direction = angle_of_index state_id }
      in 
      let rec parse_attributes att state_sig =
	match 
	  att
	with 
	| [] -> state_sig 
	| t::q -> 
	  parse_attributes q 
	    begin
	      match 
		t 
	      with 
	      | Comment _ -> state_sig 
	      | FillColor s -> {state_sig with sig_state_node = {state_sig.sig_state_node with fillcolor = s}}
	      | Color s -> {state_sig with sig_state_node = {state_sig.sig_state_node with color = s}}
	      | Fontsize i -> {state_sig with sig_state_node = {state_sig.sig_state_node with fontsize = i}}
	      | Tag (_,_) -> let _ = 
				Printf.fprintf stderr "Warning: Tags are useless in states declaration\n" 
			      in state_sig
	      | Width f -> {state_sig with sig_state_node = {state_sig.sig_state_node with width = f}}
	      | Height f -> {state_sig with sig_state_node = {state_sig.sig_state_node  with height = f}}
	      | Direction f -> 
	        {state_sig with sig_state_direction = f}
	      | Shape s -> {state_sig with sig_state_node = {state_sig.sig_state_node with shape = s}}
	      | Radius f -> parse_attributes [Width f;Height f] state_sig
	      | Set_scale f -> 
		{ state_sig with sig_state_scale_factor = f}
	      | Scale f -> parse_attributes [Set_scale (f*.state_sig.sig_state_scale_factor)] state_sig 
	    end
      in 
      let state_sig = parse_attributes attributes state_sig in 
      let site = 
	{ site 
	  with 
	    sig_site_states = 
	    snd (IdMap.add_map () () state_id state_sig site.sig_site_states)}
      in 
      let agent_type = 
	{agent_type 
	 with 
	   sig_agent_sites = snd (IdMap.add_map () ()  site_id site agent_type.sig_agent_sites)}
      in 
      (agent_id,site_id,state_id),
      {remanent with 
	agent_types = snd (IdMap.add_map () ()  agent_id agent_type remanent.agent_types)}
	
    let name_of_agent agent_id node = 
      "Agent_"^(node.label)^"_"^(string_of_int agent_id)
	
    let name_of_site agent_id agent site_id site = 
      "Site_"^(agent.label)^"_"^(string_of_int agent_id)^"_"^(site.label)^"_"^string_of_int site_id 
	
    let name_of_state agent_id agent_node site_id site state_id state = 
      "State_"^(agent_node.label)^"_"^(string_of_int agent_id)^"_"^(site.label)^"_"^(string_of_int site_id)^"_"^(state.label)^(string_of_int state_id)  
	
    let name_of_free agent_id agent_node site_id site state_id k = 
      "Free_"^(agent_node.label)^"_"^(string_of_int agent_id)^"_"^(site.label)^"_"^string_of_int site_id^"_"^(string_of_int state_id)^"_"^k
	
    let name_of_bound agent_id agent_node site_id site state_id = 
      "Bound_"^(agent_node.label)^"_"^(string_of_int agent_id)^"_"^(site.label)^"_"^string_of_int site_id^"_"^(string_of_int state_id)^"_"
	
    let point_on_countour_ext remanent node direction scale delta = 
      let angle = (remanent.config.pi *. direction)/.180. in 
      (node.abscisse +. (sin angle) *. (node.width *. 0.5 *. scale +. delta),
       node.ordinate +. (cos angle) *. (node.height *. 0.5 *. scale +. delta))
	
    let point_on_countour remanent node direction scale = 
      point_on_countour_ext remanent node direction scale 0.
	
	
    let add_tag s i map = 
      let old = 
	try 
	  TagMap.find s map 
	with 
	  Not_found -> IntSet.empty
      in TagMap.add s (IntSet.add i old) map 
      
    let add_agent agent_type abs ord attributes remanent = 
      let agent_type_info = 
	try 
	  snd (IdMap.find_map () () agent_type remanent.agent_types )
	with 
	  Not_found -> (Printf.fprintf stdout "631\n";raise Exit)
      in 
      let node_id =  remanent.nnodes + 1 in 
      let remanent = {remanent with nnodes = node_id} in 
      let node = 
	{ 
	  agent_type_info.sig_agent_node 
	  with 
	    abscisse = abs ; 
	    ordinate = ord ; 
	}
      in 
      let rec parse_attributes att node =
	match 
	  att
	with 
	| [] -> node 
	| t::q -> 
	  parse_attributes q 
	    begin
	      match 
		t 
	      with 
	      | Comment _ -> node 
	      | FillColor s -> {node with fillcolor = s}
	      | Color s -> {node with color = s}
	      | Fontsize i -> {node with fontsize = i}
	      | Tag (s,i) -> {node with tags = add_tag s i node.tags}
	      | Width f -> {node with width = f}
	      | Height f -> {node with height = f}
	      | Direction _ | Scale _ | Set_scale _ -> 
		let _ = 
		  Printf.fprintf stderr "Warning: Directions are useless in agent declaration\n" 
		in node 
	      | Shape s -> {node with shape = s}
	      | Radius f -> parse_attributes [Width f;Height f] node 
	    end
      in 
      let node = parse_attributes attributes node in 
      let node = { node with name = name_of_agent node_id node} in 
      let _ = Printf.fprintf stdout "%s" node.name in 
      let agent = {dummy_agent with 
	agent_type = agent_type ;
	agent_node = node } in 
      (node_id,agent_type),
      {remanent with 
	agents  = snd (IdMap.add_map () () node_id agent remanent.agents) ; 
	nodes = snd (IdMap.add_map () () node_id node remanent.nodes) 
      }
	
    let add_site (agent_id,agent_type) site_type attributes remanent = 
      let agent_type_id = fst site_type in 
      let _ = 
	if agent_type_id <> agent_type 
	then (Printf.fprintf stdout "683\n";raise Exit)
      in 
      let agent_type = 
	try 
	  snd (IdMap.find_map () ()  agent_type_id remanent.agent_types)
	with 
	| Not_found -> (Printf.fprintf stdout "689\n";raise Exit)
      in 
      let agent = 
	try 
	  snd(IdMap.find_map () () agent_id remanent.agents)
	with Not_found -> (Printf.fprintf stdout "694\n";raise Exit)
      in 
      let site_type_info = 
	try 
	  snd (IdMap.find_map () ()  (snd site_type) agent_type.sig_agent_sites)
	with Not_found -> (Printf.fprintf stdout "699\n";raise Exit)
      in 
      let node = 
	{ site_type_info.sig_site_node with tags = agent.agent_node.tags} 
      in 
      let direction = site_type_info.sig_site_direction in 
      let scale = site_type_info.sig_site_scale_factor in 
      let node_id = remanent.nnodes+1 in 
      let remanent = {remanent with nnodes = node_id} in 
      let rec parse_attributes att (node,direction,scale) =
	match 
	  att
	with 
	| [] -> node,direction,scale
	| t::q -> 
	  parse_attributes q 
	    begin
	      match 
		t 
	      with 
	      | Comment _ -> node,direction,scale  
	      | FillColor s -> {node with fillcolor = s},direction,scale
	      | Color s -> {node with color = s},direction,scale
	      | Fontsize i -> {node with fontsize = i},direction,scale
	      | Tag (s,i) -> {node with tags = add_tag s i  node.tags},direction,scale
	      | Width f -> {node with width = f},direction,scale
	      | Height f -> {node with height = f},direction,scale
	      | Direction f -> node,f,scale
	      | Shape s -> {node with shape = s},direction,scale
	      | Radius f -> parse_attributes [Width f;Height f] (node,direction,scale)
	      | Set_scale f -> node,direction,f
	      | Scale f -> parse_attributes [Set_scale (f*.scale)] (node,direction,scale)
	    end
      in 
      let node,direction,scale = parse_attributes attributes (node,direction,scale) in 
      let abs,ord = point_on_countour remanent agent.agent_node direction scale in 
      let node = 
	{ 
	  node with
	    abscisse = abs ; 
	    ordinate = ord ; 
	}
      in 
      let node = { node with name = name_of_site agent_id agent.agent_node node_id node} in
      let site = {dummy_site with site_node = node ; site_direction = direction} in 
      let agent = {agent with agent_sites = snd (IdMap.add_map () ()  node_id site agent.agent_sites)} in 
      (agent_id,agent_type_id,node_id,site_type),
      {remanent with 
	agents  = snd (IdMap.add_map () ()  agent_id agent remanent.agents)}
	
	
    let get_site_info  (agent_id,agent_type_id,site_id,site_type_id) remanent = 
      let agent_type = 
	try 
	  snd (IdMap.find_map () ()  agent_type_id remanent.agent_types)
	with 
	| Not_found -> (Printf.fprintf stdout "754\n";raise Exit)
      in 
      let site_type = 
	try 
	  snd (IdMap.find_map () () (snd site_type_id)  agent_type.sig_agent_sites)
	with 
	| Not_found -> (Printf.fprintf stdout "760\n";raise Exit)
      in 
      let agent = 
	try 
	  snd (IdMap.find_map () () agent_id remanent.agents)
	with Not_found -> (Printf.fprintf stdout "765\n";raise Exit )
      in 
      let site = 
	try 
	  snd (IdMap.find_map () () site_id agent.agent_sites)
	with Not_found -> (Printf.fprintf stdout "770\n";raise Exit)
      in 
      agent_type,site_type,agent,site 
	
    let add_internal_state (agent_id,agent_type_id,site_id,site_type_id) state_type attributes remanent = 
      let agent_type,site_type,agent,site = 
	get_site_info (agent_id,agent_type_id,site_id,site_type_id) remanent 
      in 
      let (_,_,state_type_id) = state_type in 
      let state_type_info = 
	try 
	  snd (IdMap.find_map () ()  state_type_id site_type.sig_site_states)
	with Not_found -> (Printf.fprintf stdout "782\n";raise Exit)
      in 
      let node = {state_type_info.sig_state_node with tags = site.site_node.tags } in 
      let direction = state_type_info.sig_state_direction in 
      let scale = state_type_info.sig_state_scale_factor in 
      let node_id = remanent.nnodes+1 in 
      let remanent = {remanent with nnodes = node_id} in 
      let rec parse_attributes att (node,direction,scale) =
	match 
	  att
	with 
	| [] -> node,direction,scale
	| t::q -> 
	  parse_attributes q 
	    begin
	      match 
		t 
	      with 
	      | Comment _ -> node,direction,scale 
	      | FillColor s -> {node with fillcolor = s},direction,scale
	      | Color s -> {node with color = s},direction,scale
	      | Fontsize i -> {node with fontsize = i},direction,scale
	      | Tag (s,i) -> {node with tags = add_tag s i node.tags},direction,scale
	      | Width f -> {node with width = f},direction,scale
	      | Height f -> {node with height = f},direction,scale
	      | Direction f -> node,f,scale
	      | Shape s -> {node with shape = s},direction,scale
	      | Radius f -> parse_attributes [Width f;Height f] (node,direction,scale)
	      | Set_scale f -> node,direction,f
	      | Scale f -> parse_attributes [Set_scale (f*.scale)] (node,direction,scale)
	    end
      in 
      let node,direction,scale = parse_attributes attributes (node,direction,scale) in 
      let abs,ord = point_on_countour_ext remanent site.site_node direction scale (node.width/.2.)  in 
      let node = 
	{ 
	  node with 
	    abscisse = abs ; 
	    ordinate = ord ; 
	}
      in 
      let site = {site with site_states = snd (IdMap.add_map () ()  node_id (Internal_node node) site.site_states)} in 
      let agent = {agent with agent_sites = snd (IdMap.add_map () ()  site_id site agent.agent_sites)} in 
      (agent_id,site_id,node_id),
      {remanent with 
	agents  = snd (IdMap.add_map () ()  agent_id agent remanent.agents);
	nodes = snd (IdMap.add_map () ()  node_id node remanent.nodes)}
	
    let op edge = {edge with 
      node1 = edge.node2 ;
      node2 = edge.node1 ;
      backward = edge.forward ; forward = edge.backward}
      
    let equ_link s1 s2 edge = 
      if s1 = s2 
      then {edge with forward = edge.forward || edge.backward ; backward = false}
      else edge
	
    let dir_of_edge e = 
      match e.backward,e.forward
      with false,false -> "none"
      | false,true -> "forward"
      | true,false -> "back"
      | true,true -> "both"
	
    let fusion_edge s1 s2 =
      {s2 with forward = s1.forward || s2.forward ; backward = s1.backward || s2.backward }
	
	
	
    let rec add_link n1 n2 edge remanent = 
      if compare n1 n2 < 0 
      then add_link n2 n1 (op edge) remanent 
      else 
	{remanent with edge_list = 
	    begin 
	      let old = 
		match  
		  snd (Id2Map.find_map_option  () ()  (n1,n2) remanent.edge_list) 
		with Some a -> a 
		| None -> []
	      in 
	      snd (Id2Map.add_map () ()  (n1,n2) (edge::old) remanent.edge_list)
	    end}
           
    let add_relation relation (*(agent_id,agent_type_id,site_id,(site_type_id:int*int))*) node1 node2
        (*(agent_id',agent_type_id',site_id',(site_type_id':int*int))*) remanent = 
      let (_,_,site_id,_) = node1 in 
      let (_,_,site_id',_) = node2 in 
      let agent_type,site_type,agent,site = 
	get_site_info node1 remanent 
      in 
      let agent_type',site_type',agent',site' = 
	get_site_info node2 remanent 
      in 
      let relation = {relation with node1 = site.site_node ; node2 = site'.site_node } in 
      add_link site_id site_id' relation  remanent 
	
    let add_match_elt (ag_id1,_) (ag_id2,_) remanent = 
      let node1 = snd(IdMap.find_map () () ag_id1 remanent.agents)  in 
      let node2 = snd(IdMap.find_map () () ag_id2 remanent.agents) in 
      add_link ag_id1 ag_id2 
	{dummy_match with node1 = node1.agent_node ; 
	  node2 = node2.agent_node} remanent 
	

    let add_proj_elt (ag1,_) (ag2,_) remanent = 
      let node1 = snd(IdMap.find_map () () ag1 remanent.agents)  in 
      let node2 = snd(IdMap.find_map () () ag2 remanent.agents) in 
      add_link ag1 ag2 
	{dummy_proj with node1 = node1.agent_node ; 
	  node2 = node2.agent_node} remanent 


    let add_edge = add_relation link 
    let add_weak_flow_and_link  x y z = add_relation weak_flow x y (add_edge x y z)
    let add_flow_and_link x y z = add_relation flow x y (add_weak_flow_and_link x y z)
    let add_strong_flow_and_link (x:site) y z = add_relation strong_flow x y (add_flow_and_link x y z)
      
    let add_weak_flow = add_relation weak_flow 
    let add_flow x y z = add_relation flow x y (add_weak_flow x y z)
    let add_strong_flow x y z = add_relation strong_flow x y (add_flow x y z)
      
    let color_of_edge e = e.color
      
    let pen_width_of edge = 
      if edge.width < 2 then "1" 
      else string_of_int edge.width 

    let add_free  (agent_id,agent_type_id,site_id,site_type_id) attributes remanent = 
      let agent_type,site_type,agent,site = 
	get_site_info (agent_id,agent_type_id,site_id,site_type_id) remanent 
      in 
      let direction = site.site_direction in 
      let node_id1 = remanent.nnodes+1 in 
      let node_id2 = node_id1+1 in 
      let node_id3 = node_id2+1 in 
      let remanent = {remanent with nnodes = node_id3} in 
      let tags = TagMap.empty in 
      let rec parse_attributes att (tags,direction,width,height) =
	match 
	  att
	with 
	| [] -> tags,direction,width,height
	| t::q -> 
	  parse_attributes q 
	    begin
	      match 
		t 
	      with 
	      | Comment _ -> tags,direction,width,height
	      | Color s -> tags,direction,width,height
	      | FillColor s -> tags,direction,width,height
	      | Fontsize i -> tags,direction,width,height (*useless*)
	      | Tag (s,i) -> add_tag s i tags,direction,width,height 
	      | Width f -> tags,direction,f,height 
	      | Height f -> tags,direction,width,f
	      | Direction f -> tags,f,width,height
	      | Shape s -> tags,direction,width,height (*useless*)
	      | Radius f -> parse_attributes [Width f;Height f] (tags,direction,width,height)
	      | Set_scale f -> tags,direction,width,height (*useless*)
	      | Scale f -> tags,direction,width,height (*useless*)
	    end
      in 
      let tags,direction,width,height = parse_attributes attributes (tags,direction,remanent.config.free_width,remanent.config.free_height) in 
      let abs1,ord1 = point_on_countour_ext remanent site.site_node direction 1. height in 
      let node1 = {dummy_node with color = "white" ; (*id = node_id1 ;*) abscisse = abs1 ; ordinate = ord1 ; tags = tags } in 
      let abs2,ord2 = point_on_countour_ext remanent node1  (direction+.90.) 1. (width/.2.)  in 
      let abs3,ord3 = point_on_countour_ext remanent node1 (direction-.90.) 1. (width/.2.)   in 
      let node2 = {dummy_node with color = "white" ; (*id = node_id2 ;*) abscisse = abs2 ; ordinate = ord2 ; tags = tags} in 
      let node3 = {dummy_node with color = "white" ; (*id = node_id3 ;*) abscisse = abs3 ; ordinate = ord3 ; tags = tags } in 
      let site = {site with site_states = 
	  snd (IdMap.add_map () ()  node_id1 (Free_nodes (node1,node2,node3)) site.site_states)}
      in 
      let agent = {agent with agent_sites = 
	  snd (IdMap.add_map () ()  site_id site agent.agent_sites)} in 
      (agent_id,site_id,node_id1),
      {remanent with 
	agents  = snd (IdMap.add_map () ()  agent_id agent remanent.agents);
	nodes = 
	  snd (IdMap.add_map () ()  node_id1 node1 
	    (snd (IdMap.add_map () ()  node_id2 node2 
	       (snd (IdMap.add_map () ()  node_id3 node3 remanent.nodes)))))}
	
	
    let add_bound  (agent_id,agent_type_id,site_id,site_type_id) attributes remanent = 
      let agent_type,site_type,agent,site = 
	get_site_info (agent_id,agent_type_id,site_id,site_type_id) remanent 
      in 
      let direction = site.site_direction in 
      let node_id = remanent.nnodes+1 in 
      let remanent = {remanent with nnodes = node_id} in 
      let tags = TagMap.empty in 
      let rec parse_attributes att (color,tags,direction,height) =
	match 
	  att
	with 
	| [] -> color,tags,direction,height
	| t::q -> 
	  parse_attributes q 
	    begin
	      match 
		t 
	      with 
	      | Comment _ -> color,tags,direction,height
	      | Color s -> s,tags,direction,height
	      | FillColor s -> color,tags,direction,height
	      | Fontsize i -> color,tags,direction,height (*useless*)
	      | Tag (s,i) -> color,add_tag s i tags,direction,height 
	      | Width f -> color,tags,direction,height (*useless*)
	      | Height f -> color,tags,direction,f
	      | Direction f -> color,tags,f,height
	      | Shape s -> color,tags,direction,height (*useless*)
	      | Radius f -> parse_attributes [Height f] (color,tags,direction,height)
	      | Set_scale f -> color,tags,direction,height (*useless*)
	      | Scale f -> color,tags,direction,height (*useless*)
	    end
      in 
      let color,tags,direction,height = parse_attributes attributes ("black",tags,direction,remanent.config.bound_height) in 
      let abs,ord = point_on_countour_ext remanent site.site_node direction 1. height in 
      let node = {dummy_node with color = color ;  (*id = node_id ;*) abscisse = abs ; ordinate = ord ; tags = tags } in 
      let site = {site with site_states = 
	  snd (IdMap.add_map () ()  node_id (Bound_node (node)) site.site_states)}
      in 
      let agent = {agent with agent_sites = snd (IdMap.add_map () ()  site_id site agent.agent_sites)} in 
      (agent_id,site_id,node_id),
      {remanent with 
	agents  = snd (IdMap.add_map () ()  agent_id agent remanent.agents);
	nodes = 
	  snd (IdMap.add_map () ()  node_id node  remanent.nodes)}
	
    let filter_label_in_agent remanent label =
      if remanent.config.show_agent_names then label else "" 
    let filter_color_in_agent remanent color = 
      if remanent.config.color_agents then color else "white"
    let filter_label_in_site remanent label =
      if remanent.config.show_site_names then label else "" 
    let filter_color_in_site remanent color = 
      if remanent.config.color_sites then color else "white"
    let filter_label_in_state remanent label =
      if remanent.config.show_state_names then label else "" 
    let filter_color_in_state remanent color = 
      if remanent.config.color_states then color else "white"
	
    let filter_tags list map = 
      let def l deft p s_of_x = 
	List.for_all 
	  (fun x ->
	    try 
	      let s = s_of_x x in 
	      let set = TagMap.find s map in 
	      p x set 
	    with 
	      Not_found -> deft)
	  l 
      in 
      def list true (fun (s,i) set -> IntSet.mem i set) fst 
	
    let dump_edge log (s1,s2) edge remanent = 
      Printf.fprintf log "%s -> %s [dir = \"%s\",color=\"%s\",penwidth=%s,label=\"%s\",fontsize=%i,style=\"%s\"];\n" s1 s2 (dir_of_edge edge) (color_of_edge edge) (pen_width_of edge) (edge.comment) (remanent.config.edge_label_font) (edge.style)
	
	
    let dump_node log filter filter_label filter_color node remanent = 
      Printf.fprintf log 
	"fixedsize=true,label = \"%s\",\nfontsize=%i,\npos=\"%f,%f!\",\nwidth=%f,\nheight=%f,\nshape=\"%s\",\nstyle=filled,\nfillcolor=%s,\ncolor=%s\n" (filter_label node.label) node.fontsize node.abscisse node.ordinate node.width node.height node.shape (filter_color node.fillcolor) node.color
	
    let dump_int_state chan filter remanent agent_id agent site_id site state_id state =
      let tags = state.tags in 
      if filter_tags filter tags 
      then 
	let _ = Printf.fprintf chan "%s [\n" (name_of_state agent_id agent.agent_node site_id site.site_node state_id state) in
	let _ = dump_node chan filter (filter_label_in_state remanent) (filter_color_in_state remanent) state remanent in 
	let _ = Printf.fprintf chan "]\n\n" in 
	()
	  
    let dump_state chan filter remanent agent_id agent site_id site state_id state = 
      match state 
      with 
      | Internal_node state -> dump_int_state chan filter remanent agent_id agent site_id site state_id state
      | Free_nodes (node1,node2,node3) -> 
	let name0 = name_of_site agent_id agent.agent_node site_id site.site_node in 
	let name1 = name_of_free  agent_id agent.agent_node site_id site.site_node state_id "1" in
	let name2 = name_of_free  agent_id agent.agent_node site_id site.site_node state_id "2" in
	let name3 = name_of_free  agent_id agent.agent_node site_id site.site_node state_id "3" in
	let _ = Printf.fprintf chan "%s [\n" name1 in 
	let _ = dump_node chan filter (fun _ -> "") (fun x -> x) node1 remanent in 
	let _ = Printf.fprintf chan "]\n%s [\n" name2 in 
	let _ = dump_node chan filter (fun _ -> "") (fun x -> x) node2 remanent in 
	let _ = Printf.fprintf chan "]\n%s [\n" name3 in 
	let _ = dump_node chan filter (fun _ -> "") (fun x -> x) node3 remanent in 
	let _ = Printf.fprintf chan "]\n" in 
	let _ = dump_edge chan (name0,name1) {link with node1 = node1 ; node2 = site.site_node} remanent in 
	let _ = dump_edge chan (name2,name3) {link with node1 = node2 ; node2 = node3} remanent in 
	let _ = Printf.fprintf chan "\n" in ()
					 
      | Bound_node node1 -> 
	let name0 = name_of_site agent_id agent.agent_node site_id site.site_node in 
	let name1 = name_of_bound  agent_id agent.agent_node site_id site.site_node state_id in
	let _ = Printf.fprintf chan "%s [\n" name1 in 
	let _ = dump_node chan filter (fun _ -> "") (fun x -> x) node1 remanent in 
	let _ = Printf.fprintf chan "]\n" in 
	let _ = dump_edge chan (name0,name1) {link with node1 = node1 ; node2 = site.site_node ; color = node1.color} remanent in 
	let _ = Printf.fprintf chan "\n" in ()
					 
					 
    let dump_site chan filter remanent agent_id agent site_id site = 
      let tags = site.site_node.tags in 
      if filter_tags filter tags 
      then 
	let _ = Printf.fprintf chan "%s [\n" (name_of_site agent_id agent.agent_node site_id site.site_node) in
	let _ = dump_node chan filter (filter_label_in_site remanent) (filter_color_in_site remanent) site.site_node remanent in 
	let _ = Printf.fprintf chan "]\n\n" in 
	let _ = IdMap.iter_map   (dump_state chan filter remanent agent_id agent site_id site ) site.site_states in 
	()
	  
    let dump_agent chan filter remanent agent_id agent = 
      let tags = agent.agent_node.tags in 
      if filter_tags filter tags 
      then 
	let _ = Printf.fprintf chan "%s [\n" (name_of_agent agent_id agent.agent_node) in 
	let _ = dump_node chan filter (filter_label_in_agent remanent) (filter_color_in_agent remanent) agent.agent_node remanent in 
	let _ = Printf.fprintf chan "]\n\n" in 
	let _ = IdMap.iter_map (dump_site chan filter remanent agent_id agent) agent.agent_sites in 
	()
	  
    let dump_edge_list chan filter remanent (n1,n2) l = 
      let l = List.filter 
	(fun edge -> (filter_tags filter edge.node1.tags && 
			filter_tags filter edge.node2.tags && 
			filter_tags filter edge.edges_tag )) l
      in 
      let threshold  = 
	List.fold_left
	  (fun threshold l -> 
	    max threshold l.priority
	  )
	  0 l  
      in 
      let rec aux l accu  = 
	match 
	  l 
	with 
	  edge::q -> 
	    if edge.priority < threshold
	    then aux q accu 
	    else 
	      begin
	    let accu  =
	      match accu
	      with None -> Some edge
	      | Some accu -> 
		Some (fusion_edge accu edge)
	    in 
	    aux q accu 
	      end
	| [] -> accu
      in 
      match 
	aux l None
      with 
	None -> ()
      | Some edge -> dump_edge chan (edge.node1.name,edge.node2.name) edge remanent 
	
    let dump file filter remanent = 
      let chan = open_out file in 
      let _ = Printf.fprintf chan "digraph G {\n\n" in 
      let _ = 
	IdMap.iter_map 
	  (dump_agent chan filter remanent) 
	  remanent.agents
      in 
      let _ = 
	Id2Map.iter_map 
	  (dump_edge_list chan filter remanent)
	  remanent.edge_list  
      in 
      let _ = Printf.fprintf chan "}\n" in 
      let _ = close_out chan in 
      ()
	
    let new_state agent (site:site) (remanent,state_list) state =       let id,remanent = 
	match state
	with 
	  Free op  -> add_free site op remanent
	| Bound op -> add_bound site op remanent 
	| Internal (op1,op2) -> add_internal_state site op1 op2  remanent 
      in 
      remanent,id::state_list 
	
    let new_site agent (remanent,site_list) (site,directives,states) = 
      let site,remanent = add_site agent site  directives remanent in 
      let remanent,states  = List.fold_left (new_state agent site) (remanent,[]) states in 
      remanent,((site,List.rev states)::site_list)
	
    let new_agent (remanent,agent_list) (ag,op1,op2,op3,l)  = 
      let agent,remanent = add_agent ag op1 op2 op3 remanent in 
      let remanent,sites = List.fold_left (new_site agent) (remanent,[]) l in 
      remanent,(agent,List.rev sites)::agent_list
	
    let add_in_graph (remanent:remanent_state) (l:graph) = 
      let remanent,l = 
	List.fold_left new_agent (remanent,[]) l
      in 
      remanent, (List.rev l:graph_vars)
	
	
    let edge_list f l remanent = 
      List.fold_left (fun remanent (x,y) -> f x y remanent) remanent l
	
    let add_flow_list = edge_list add_flow 
    let add_flow_and_link_list = edge_list add_flow_and_link 
    let add_strong_flow_list = edge_list add_strong_flow 
    let add_strong_flow_and_link_list = edge_list add_strong_flow 
    let add_link_list = edge_list add_edge 
    let add_weak_flow_list = edge_list add_weak_flow 
    let add_weak_flow_and_link_list = edge_list add_weak_flow_and_link 

    let add_free_list l remanent = 
      List.fold_left 
	(fun (list,remanent) (site,directives) -> 
	 let a,remanent = add_free site directives remanent in 
	 a::list,remanent)
	([],remanent)
	l 
	  
    
    let fold_id_and_nodes f remanent = 
      IdMap.fold_map f remanent.nodes
	
    let fold_id f = fold_id_and_nodes (fun x _ -> f x)
    let fold_node f = fold_id_and_nodes (fun _ y -> f y)

    let map_node_in_state f state = 
      match state with 
      | Internal_node n -> Internal_node (f n)
      | Free_nodes (n1,n2,n3) -> Free_nodes (f n1,f n2,f n3)
      | Bound_node n -> Bound_node (f n)
	
    let map_node_in_sites f g site = 
      {
	 site_node = f site.site_node ;
	 site_direction = g site.site_direction ; 
	 site_states = 
	  IdMap.map_map (map_node_in_state f) site.site_states}
    let map_node_in_agent f g agent = 
      {agent 
       with 
	 agent_node = f agent.agent_node ;
	 agent_sites = 
	  IdMap.map_map (map_node_in_sites f g) agent.agent_sites}
	
    let map_node f_node f_direction remanent = 
      {remanent 
       with 
	 agents = 
	  IdMap.map_map (map_node_in_agent f_node f_direction) remanent.agents ; 
(*         nodes = IdMap.map_map f_node remanent.nodes; 
         edge_list = 
             Id2Map.map_map (List.map (fun edge -> {edge with node1 = f_node edge.node1 ; node2 = f_node edge.node2 }))
	       remanent.edge_list*)
      }

    let map_id f_id remanent = (*bug ne passe pas dans les frees *)
      let nodes,nnodes = 
	IdMap.fold_map
	  (fun id node (map,nnodes) -> 
	    snd (IdMap.add_map () () (f_id id) node map),
	    max (f_id id) nnodes)
	  remanent.nodes
	  (IdMap.empty_map,0) 
      in 
      let nnodes = max (f_id remanent.nnodes) (nnodes+1) in 
      let agents,name_map = 
	IdMap.fold_map 
	  (fun id agent (map,map2) -> 
	    let ag_id = f_id id in 
	    let name = name_of_agent ag_id agent.agent_node in 
	    let agent_node = {agent.agent_node with 
	      name = name }
	    in 
	    let map2 = 
	      snd (IdMap.add_map () () ag_id name map2) 
	    in 
	    let agent_sites,map2 = 
	      IdMap.fold_map 
		(fun id site (map,map2) -> 
		  let site_id = f_id id in 
		  let name = name_of_site ag_id agent_node site_id site.site_node in 
		  let site = 
		    {site with 
		      site_node = {site.site_node 
				   with name = name}}
		  in 
		  snd (IdMap.add_map () () site_id site map),
		  snd (IdMap.add_map () () site_id name map2))
		agent.agent_sites (IdMap.empty_map,map2)
	    in 
	    let agent = 
	      { 
		agent 
		with 
		  agent_node = agent_node ;
		  agent_sites = agent_sites 
	      }
	    in 
	    snd (IdMap.add_map () ()  (f_id id) agent map),
	    map2)
	  remanent.agents (IdMap.empty_map,IdMap.empty_map)
      in
      let edge_list = 
	Id2Map.fold_map
	  (fun (id1,id2) y z -> 
	    let y = 
	      List.map 
		(fun y -> 
		  try 
		    {y with node1 = 
			{y.node1 with name = snd (IdMap.find_map () () (f_id id1) name_map)} ;
		    node2 = 
			{y.node2 with name = snd (IdMap.find_map () () (f_id id2) name_map)}}
		  with _ -> y)
		y 
	    in 
	      
	    snd (Id2Map.add_map () ()  (f_id id1,f_id id2) y z))
	  remanent.edge_list Id2Map.empty_map
      in  
      let remanent = 
	{remanent with 
	  nodes = nodes;
	  nnodes = nnodes;
	  agents = agents ;
	  edge_list = edge_list ;
	}
      in 
      let rename_agent = (fun (id,x) -> (f_id id,x)) in 
      let rename_site = (fun (id1,x,id2,y) -> (f_id id1,x,f_id id2,y)) in 
      let rename_state = (fun (x,y,z) -> (f_id x,f_id y,f_id z)) in 
      rename_agent,rename_site,rename_state,remanent 

   

	
    let add_to_id n remanent = 
      map_id (fun x -> if x = 1 then 1 else x+n) remanent 
      

    let max_id remanent = fold_id max remanent 0 
    let corners remanent = 
      let fusion (x,y,z,t) (x',y',z',t') = (min x x',max y y',min z z',max t t') 
      in 
      fold_node 
	(fun node pos ->
	  let x,y=node.abscisse,node.ordinate in 
	  let coords =
	    x-.node.width,x+.node.width,y-.node.height,y+.node.height
	  in 
	  match pos
	  with None -> Some coords
	  | Some old -> Some (fusion coords old))
	remanent 
	None

    let size remanent = 
      match corners remanent
      with None -> 0.,0.
      | Some (x,y,z,t) -> y-.x,t-.z
	

    let rotate x y  angle remanent = remanent 
    let translate x y remanent = 
      map_node 
	(fun node -> 
	  {node with abscisse = node.abscisse +. x;
	             ordinate = node.ordinate +. y}
	) (fun x -> x) remanent 
    let sym_h y remanent = remanent 
    let sym_v x remanent = remanent 
    let sym x y remanent = remanent 
    let horizontal_swap remanent = 
      match corners remanent 
      with 
      | None -> remanent 
      | Some (_,_,y_min,y_max) -> 
	let axe = (y_max +. y_min) /. 2. in 
	map_node 
	  (fun node -> 
	    {node with ordinate = 2.*.axe -. node.ordinate})
	  (fun angle -> 180. -. angle)
	  remanent 
    let vertical_swap remanent = 
      match corners remanent 
      with 
      | None -> remanent 
      | Some (x_min,x_max,_,_) -> 
	let axe = (x_max +. x_min) /. 2. in 
	map_node 
	  (fun node -> 
	    {node with abscisse = 2.*.axe -. node.abscisse})
	  (fun angle -> 360. -. angle)
	  remanent 
	  
    let fuse fold2map addmap f map1 map2 = 
       snd (
	    fold2map
	      () () 
	      (fun x y z map -> 
		f x y z map)
	      (fun x y map -> map)
	      (fun x z map -> 
		addmap () () x z (snd map))
	      map1 
	      map2
	      map1)

    let fuse remanent remanent' = 
      let _ = Printf.fprintf stdout "FUSE %i %i \n" remanent.nnodes remanent'.nnodes in 
      if not (remanent.config == remanent'.config)
      then (Printf.fprintf stdout "1334\n";raise Exit)
      else 
	let nnodes = max remanent.nnodes remanent'.nnodes in 
	let nodes = fuse IdMap.fold2_map IdMap.add_map 
	  (fun x y z map -> if y==z then map else (Printf.fprintf stderr "1338\n";raise Exit))
	  remanent.nodes remanent'.nodes in 
	let nodes = remanent.nodes in 
	let fuse_hier l1 g1 l2 g2 a b = 
	  fuse IdMap.fold2_map IdMap.add_map 
	    (fun x y z map -> 
	      g1 x y z (
		fuse IdMap.fold2_map IdMap.add_map
		  (fun x y z map -> 
		    g2 x y z (fuse IdMap.fold2_map IdMap.add_map
				(fun x y z map ->
				  if y==z then map else 
				    let _ = Printf.fprintf stdout "1350\n" in raise Exit)
				(l2 y) (l2 z)) map)
		  (l1 y) (l1 z)) map)
	    a b 
	in 
	let agents = 
	  fuse_hier 
	    (fun x -> x.agent_sites) 
	    (fun x y z agent_sites map -> 
	      if not (y.agent_type ==z.agent_type && y.agent_node == z.agent_node)
	      then (Printf.fprintf stdout "1360";raise Exit)
	      else IdMap.add_map () () x {y with agent_sites = agent_sites} (snd map))
	    (fun x -> x.site_states)
	    (fun x y z site_states map -> map)
	    remanent.agents remanent'.agents 
	in 
	let agent_types = 
	  fuse_hier 
	    (fun x -> x.sig_agent_sites) 
	    (fun x y z sig_agent_sites map -> 
	      if not (y.sig_agent_node == z.sig_agent_node)
	      then raise Exit 
	      else 
		IdMap.add_map () () x 
		  {y 
		   with 
		     sig_agent_sites = sig_agent_sites ; 
		     sig_agent_n_sites = max y.sig_agent_n_sites z.sig_agent_n_sites} 
		  (snd map))
	    (fun x -> x.sig_site_states)
	    (fun x y z site_states map -> map)
	    remanent.agent_types remanent'.agent_types
	in 
	let edge_list = 
	  snd (Id2Map.fold2_map
	    ()
	    ()
	    (fun x y z map -> 
	      if not(y==z) then map
	      else let _ = Printf.fprintf stdout "1407\n" in 
		   raise Exit)
	    (fun x y z -> z)
	    (fun x y map -> Id2Map.add_map () () x y (snd map))
   	    remanent.edge_list 
	    remanent'.edge_list
	    remanent.edge_list   
	  )
	in
	let _ = Printf.fprintf stdout "FUSE: %i\n" nnodes in 
	{ 
	  remanent 
	  with nnodes = nnodes ; 
	    nodes = nodes ;
	    agent_types = agent_types ;
	    agents = agents ;
	    edge_list = edge_list
	}

    let disjoint_union remanent remanent' = 
      let delta = remanent.nnodes in 
      let _ = Printf.fprintf stdout "disjoint_union %i %i\n" delta remanent'.nnodes in 
      let (f,g,h,remanent') = add_to_id delta remanent' in 
      (fun x->x),
	  (fun y->y),
	  (fun z->z),
	  f,g,h,
      fuse remanent remanent'
    
      
    let add_match l remanent =  
      List.fold_left 
	(fun remanent (x,y) -> add_match_elt x y remanent) remanent l 
	

    let add_proj l remanent = 
      List.fold_left 
	(fun remanent (x,y) -> add_proj_elt x y remanent) remanent l 
	
    let add_emb list remanent = remanent
    let disjoint_union_with_match remanent remanent = 
      (fun x -> x),(fun x -> x),(fun x -> x),(fun x->x),(fun x->x),(fun x->x),remanent 

    let move_remanent_right_to delta remanent remanent' = 
      	match corners remanent,corners remanent'
	with None, _ -> remanent
	| _,None -> remanent
	| Some (xmin,xmax,ymin,ymax),Some (xmin',xmax',ymin',ymax') -> 
	  translate (xmax'-.xmin+.delta) 0. remanent 
	    
    let move_remanent_left_to delta remanent remanent' = 
      	match corners remanent,corners remanent'
	with None, _ -> remanent
	| _,None -> remanent
	| Some (xmin,xmax,ymin,ymax),Some (xmin',xmax',ymin',ymax') -> 
	  translate (xmin'-.xmax-.delta) 0. remanent 


    let move_remanent_above delta remanent remanent' = 
      	match corners remanent,corners remanent'
	with None, _ -> remanent
	| _,None -> remanent
	| Some (xmin,xmax,ymin,ymax),Some (xmin',xmax',ymin',ymax') -> 
	  translate 0. (ymax'-.ymin+.delta) remanent 
	    
    let move_remanent_bellow delta remanent remanent' = 
      	match corners remanent,corners remanent'
	with None, _ -> remanent
	| _,None -> remanent
	| Some (xmin,xmax,ymin,ymax),Some (xmin',xmax',ymin',ymax') -> 
	  translate 0. (ymin'-.ymax-.delta) remanent 
	    
    let init config = 
      let remanent = init config in 
	add_agent_type 
	  "" 
	  [Width (0.);Height(0.);FillColor("white");Color("white")]
	  remanent

    let dummy_node_type = 1 
	  
    let add_rule x y directives remanent = 
        let rec parse_attributes att (comment,width,size,direction,tags,color) = 
	match 
	  att
	with 
	| [] -> comment,width,size,direction,tags,color 
	| t::q -> 
	  parse_attributes q 
	    begin
	      match 
		t 
	      with 
	      | Comment s -> s,width,size,direction,tags,color
	      | Color s -> comment,width,size,direction,tags,s 
	      | FillColor s -> comment,width,size,direction,tags,color 
	      | Fontsize i -> comment,direction,size,direction,tags,color
	      | Tag (s,i) -> comment,width,size,direction,add_tag s i tags,color
	      | Width f -> comment,f,size,direction,tags,color
	      | Height f -> comment,width,f,direction,tags,color
	      | Direction f -> comment,width,size,f,tags,color
	      | Shape s -> comment,width,size,direction,tags,color
	      | Radius f -> comment,f,f,direction,tags,color
	      | Set_scale f -> comment,width,size,direction,tags,color
	      | Scale f ->comment,width,size,direction,tags,color
	    end
	in 
	let _ = Printf.fprintf stdout "NRULES %i\n" remanent.nnodes in 
	let comment,width,size,direction,tags,color = 
	  parse_attributes 
	    directives 
	    ("",float_of_int (remanent.config.rule_width),remanent.config.rule_length,e,TagMap.empty,"black")
	in 
	let width = int_of_float width in 
	let angle = (remanent.config.pi *. direction)/.180. in 
     	let x1 = x +. size *. (sin angle) /. 2. in 
	let y1 = y +. size *. (cos angle) /. 2. in 
	let x2 = x -. size *. (sin angle) /. 2. in 
	let y2 = y -. size *. (cos angle) /. 2. in 
	let (node_id1,_),remanent = 
	  add_agent dummy_node_type x1 y1 directives remanent 
	in 
	let (node_id2,_),remanent = 
	  add_agent dummy_node_type x2 y2  directives remanent
	in 
	let node1 = 
	  snd 
	    (IdMap.find_map () () node_id1 remanent.nodes)
	in 
	let node2 = 
	  snd
	    (IdMap.find_map () () node_id2 remanent.nodes)
	in 
	add_link 
	  node_id2 
	  node_id1 
	  {rule with width = width ; comment = comment ;
	  color = color ; edges_tag = tags ; node1 = node2 ; node2 = node1 } remanent 
	  
    let cross remanent = 
      let comment = "" in 
      let directives = [] in 
      match corners remanent 
      with 
      | None -> remanent 
      | Some (x,x',y,y') -> 
	let (node_id1,_),remanent = 
	  add_agent dummy_node_type x y' directives remanent 
	in 
	let (node_id2,_),remanent = 
	  add_agent dummy_node_type x' y  directives remanent
	in 
	let (node_id3,_),remanent = 
	  add_agent dummy_node_type x y directives remanent 
	in 
	let (node_id4,_),remanent = 
	  add_agent dummy_node_type x' y'  directives remanent
	in 
	let node1 = 
	  snd 
	    (IdMap.find_map () () node_id1 remanent.nodes)
	in 
	let node2 = 
	  snd
	    (IdMap.find_map () () node_id2 remanent.nodes)
	in 
	let node3 = 
	  snd 
	    (IdMap.find_map () () node_id3 remanent.nodes)
	in 
	let node4 = 
	  snd
	    (IdMap.find_map () () node_id4 remanent.nodes)
	in 
	add_link 
	  node_id3
	  node_id4
	  {rule with 
	    forward = false ;width = 5 ; comment = comment ;
	    color = "red" ; edges_tag = TagMap.empty ; node1 = node3 ; node2 = node4 }
	(
	  add_link 
	    node_id2 
	    node_id1 
	    {rule with forward = false ;width = 5 ; comment = comment ;
	      color = "red" ; edges_tag = TagMap.empty ; node1 = node2 ; node2 = node1 } remanent )
	    

    let tag_all_nodes t i remanent = 
      let remanent = 
	map_node 
	  (fun node -> 
	    {node with tags = add_tag t i node.tags})
	  (fun x -> x)
	  remanent 
      in 
      {
      remanent with 
	edge_list = 
	  Id2Map.map_map (List.map (fun x -> {x with edges_tag = add_tag t i x.edges_tag})) remanent.edge_list
      }

   end:GKappa)
    
