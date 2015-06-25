(**
 * gkapa.ml 
 * GKappa
 * Jérôme Feret, projet Antique, INRIA Paris-Rocquencourt
 * 
 * Creation:                      <2015-03-28 feret>
 * Last modification: Time-stamp: <2015-06-25 15:08:04 feret>
 * * 
 *  
 * Copyright 2015 Institut National de Recherche en Informatique  * et en Automatique.  All rights reserved.  
 * This file is distributed under the terms of the 
 * GNU Library General Public License *)


(* GKappa is a OCAML library to help making slides in Kappa *)
(* More applications are coming soon (hopefully) *)


open Geometry

type in_out = Inside | Outside 

let escaped s i = 
  let s = String.escaped s in 
  let n = String.length s in 
  let rec aux i k l = 
    if i=n then l 
    else 
      if s.[i]='\\'
      then 
	if k then aux (i+1) false (i::l)
	else aux (i+1) true l
      else 
	aux (i+1) false l 
  in 
  let list = List.rev (aux 0 false []) in 
  let rec aux k list list2 = 
    match list with 
    | t::q -> 
	aux (t+1) q ((String.sub s k (t-k))::list2)
    | [] -> List.rev ((String.sub s k (n-k))::list2)
  in 
  let s = String.concat "" (aux 0 list []) in 
  match i with None -> s 
  | Some i -> if n = 0 then s 
  else if String.sub s 0 1 = "$"
  then 
    let n = String.length s in 
    let s = String.sub s 1 (n-2) in 
    "${\\fontsize{"^(string_of_int i)^"}{"^(string_of_int (int_of_float(floor  (float_of_int i*.1.2))))^"}\\selectfont \\ladottext{"^s^"}}$"
  else s
    

(*%module GKappa = 
%  struct 
*)
type tag = string 
type directive = 
| Fontsize of int 
| Tag of tag * int  
| Radius of float 
| Width of float 
| Height of float 
| Direction of angle
| Shape of string 
| Set_scale of float 
| Scale of float 
| Color of string 
| FillColor of string 
| Comment of string 
type id = int 
type agent_type = id 
type site_type = id 
type internal_state_type = id 
type agent = id  
type site = id 
type state = id  
type state_type = 
| Free_site of directive list 
| Bound_site of directive list
| Internal_state of internal_state_type * directive list

type signature_vars = (agent_type * (site_type * internal_state_type list) list) list 
type signature = 
  (string * directive list * 
     (string * directive list * 
	((string * directive list) list)) list) list 

type graph_vars = (agent * (site * state list ) list ) list 
type graph = 
  (agent_type *float*float*directive list* 
     (site_type * directive list* 
	state_type list) list) list

type valuation = graph_vars * (site * state list) list * state list 
    
type lift = (agent -> agent) * (site -> site) * (state -> state)
    
let init_id = 0 
let succ x = x+1 
let dummy_id = -1 
  
module TagMap = Data_structures.Make (struct type t = tag let compare = compare end)
module IntSet = Set.Make (struct type t = int let compare = compare end)
module Id = struct type t = id let compare = compare end
module IdMap = Data_structures.Make(Id)
module IdSet = Set.Make(Id)
module Id2Map = Data_structures.Make (struct type t = id * id let compare = compare end)
module StringMap = Data_structures.Make (struct type t = string let compare = compare end)
module String2Map = Data_structures.Make (struct type t = string * string let compare = compare end) 
  
type config = 
  { 
    show_agent_names: bool;
    show_site_names: bool;
    show_state_names: bool;
    show_free_symbols: bool;
    color_agents: bool;
    color_sites: bool;
    color_states: bool ;
    site_width: float ;
    site_height: float ;
    state_width: float ;
    state_height: float ;
    agent_width: float;
    agent_height: float;
    agent_colors: string list ;
    site_colors: string list ;
    state_colors: string list ;
    pi : float ;
    free_width : float ;
    free_height : float ;
    bound_height : float ; 
    rule_length: float ;
    rule_width: int;
    link_width: int;
    pairing_width: int;
    projection_width: int;
    cross_width: int;
    txt_font: int;
    agent_label_font: int; 
    site_label_font: int;
    state_label_font: int;
    dummy_font: int;
    edge_label_font: int;
    rule_color:string;
    rule_style:string;
    projection_color:string ;
    projection_style: string;
    pairing_style: string;
    pairing_color:string ;
    weak_flow_style: string;
    weak_flow_color:string ;
    weak_flow_width:int;
    strong_flow_style: string;
    strong_flow_color:string ;
    flow_style: string;
    flow_color:string ;
    rule_margin: float;
    flow_padding: float;
    flow_width: int;
    strong_flow_width: int;
  }
    
type intset = IntSet.t 
type sig_kind = Agent_type | Site_type | State_type 
type node_kind = Agent of id | Site of id | State of id | Rule_source | Rule_target | Free of int | Bound | Dummy_node 
type edge_kind = Link | Relation of (node_kind * node_kind) | Free_symbol of int | Bound_symbol  | Rule | Dummy_edge

let assert_compatible_items x y = true (*to do*)

let string_of_node_kind x = 
  match x
  with 
  | Agent _ -> "Agent"
  | Site _ -> "Site"
  | State _ -> "State"
  | Rule_source -> "RuleS"
  | Rule_target -> "RuleT"
  | Dummy_node -> "Dummy"
  | Free i -> "Free"^(string_of_int i)
  | Bound -> "Bound"

let string_of_edge_kind x =
  match x 
  with 
  | Link -> "Link"
  | Relation (_,_) -> "Relation"
  | Rule -> "Rule"

let string_of_sig_type x =
  match x 
  with 
  | Agent_type -> "Agent_type"
  | Site_type -> "Site type"
  | State_type -> "State type"

type 'a item = 
  { 
    label: string ; 
    kind: 'a ;
    sibblings: IdSet.t IdMap.t; 
    n_sibblings: Id.t;
    father: id option; 
    tags: intset TagMap.t ;
    width: float ;
    height: float ;
    fontsize: int ; 
    shape: string ; 
    fillcolor:string ;
    color:string;
    coordinate: point;
    orientation: angle;
    scale_factor: float;
    forward: bool;
    backward: bool; 
    priority: int;
    style: string;
    comment: string ;
  }

let dummy_item = 
  { 
    forward = false ;
    backward = false ;
    priority = 1;
    style = "";
    comment = "";
    kind = Dummy_node ;
    father = None ;
    sibblings = IdMap.empty ; 
    label = "" ;
    tags = TagMap.empty; 
    n_sibblings=0;
    width = 0.;
    height = 0.;
    fontsize = 0; 
    fillcolor = "white";
    color = "black";
    coordinate = origin;
    shape = "ellipse";
    scale_factor=1.;
    orientation=n;
    
  }
    
let dummy_agent_type config =
  { 
    dummy_item 
    with
      kind = Agent_type ;
      style = "filled";
      width = config.agent_width;
      height = config.agent_height;
      fontsize = config.agent_label_font ; 
  }
	
let dummy_site_name config = 
  { 
    dummy_item with 
      kind = Site_type ;
      style = "filled";
      width = config.site_width ;
      height = config.site_height ;
      fontsize = config.site_label_font; 
      scale_factor = 1.05
  }
	
let dummy_state_kind config = 
  { 
    dummy_item with 
      kind = State_type ;
      style = "filled"; 
      width = config.state_width ;
      height = config.state_height ;
      fontsize = config.state_label_font ;
  }
	
let link config = 
  {
    dummy_item 
   with kind = Link ; 
     width = (float_of_int config.link_width)
  }
let pairing config = 
  {
    (link config)
   with style = config.pairing_style ; 
     color = config.pairing_color ; 
     width = (float_of_int config.pairing_width)}
let projection config = 
  {
    (pairing config) 
   with forward = true ; 
     style = config.projection_style ; 
     color = config.projection_color ; 
     width = (float_of_int config.projection_width)}
let rule config = 
  { (link config)
    with forward = true ; 
      kind = Rule ; 
      width = (float_of_int config.rule_width) ; 
      color = config.rule_color ; style = config.rule_style }
let weak_flow config = 
  { (link config)
    with 
      priority = 2;
      forward = true;
      color = config.weak_flow_color;
      style = config.weak_flow_style ;
      width = (float_of_int config.weak_flow_width); 
      tags = TagMap.add "flow" (IntSet.add 1 (IntSet.add 2 IntSet.empty)) TagMap.empty}

let flow config = 
  { (weak_flow config) 
    with 
      priority = 3;
      color = config.flow_color;
      style = config.flow_style ;
      width = (float_of_int config.flow_width); 
      tags = TagMap.add "flow" (IntSet.add 1 IntSet.empty) TagMap.empty}
	
let strong_flow config = 
  { (flow config)
    with 
    color = config.strong_flow_color;
    style = config.strong_flow_style ;
    width = (float_of_int config.strong_flow_width); 
    tags = TagMap.add "flow" (IntSet.add 2 IntSet.empty) TagMap.empty}
	
  
let dummy_agent = dummy_agent_type
let dummy_site = dummy_site_name
let dummy_state = dummy_state_kind
      
type remanent_state = 
  {
    config: config ;
    items: node_kind item IdMap.t;
    edges: edge_kind item list Id2Map.t;
    nagent_sig_items: int;
    nsig_items: int ;
    sig_items:sig_kind item IdMap.t ;
    nitems: id;
    agents: IdSet.t IdMap.t
  }
	

let dummy_txt_item config = 
  { 
    dummy_item with 
      width = 1.;
      height = 1.;
      fontsize = config.txt_font;
      priority = 1;
      style = "";
      comment = "";
      kind = Dummy_node ;
      father = None ;
      sibblings = IdMap.empty ; 
      label = "" ;
      tags = TagMap.empty; 
      n_sibblings=0;
      shape = "plaintext";
      scale_factor=1.;
      orientation=n}

let dummy_agent_item = 
  { 
    dummy_item with 
      width = 2.;
      height = 1.;
      fontsize = 30; 
  }
    
let dummy_site_item = 
  { 
    dummy_item with 
      width = 2.;
      height = 1.;
      fontsize = 14; 
  }
	
let dummy_state_item = 
  { 
    dummy_item with 
      width = 0.4;
      height = 0.4;
      fontsize = 10; 
  }

let init config = 
  { 
    config = config ;
    items = IdMap.empty ;
    sig_items = IdMap.empty ;
    edges = Id2Map.empty ;
    nitems = init_id;
    nagent_sig_items = init_id;
    nsig_items = init_id ;
    agents = IdMap.empty
  }

let add_tag s i map = 
      let old = 
	match 
	  TagMap.find_option s map 
	with 
	| None -> IntSet.empty
	| Some a -> a
      in 
      TagMap.add s (IntSet.add i old) map 

let print_warn_attribute log t s = 
  Printf.fprintf stderr "Warning: %s directive is useless in %s declaration\n" 
    (
      match t with 
	  | Comment _ -> "Comment" 
	  | FillColor _ -> "Fill_color"
	  | Color _ -> "Color"
	  | Fontsize _ -> "Fontsize"
	  | Tag _ -> "Tag"
	  | Width _ -> "Width"
	  | Height _ -> "Height"
	  | Direction _ -> "Direction"
	  | Shape _ -> "Shape"
	  | Radius _ -> "Radius"
	  | Scale _ | Set_scale _ -> "Scale")
    s

let rec parse_attributes p s att item = 
  match 
    att
  with 
  | [] -> item
  | t::q -> 
    let item  = 
      if not (p t)
      then 
	let _ = print_warn_attribute stderr t s in 
	item 
      else 
	begin 
	  match t with 
	  | Comment s -> {item with comment = escaped s None }
	  | FillColor s -> {item with fillcolor = s}
	  | Color s -> {item with color = s}
	  | Fontsize i -> {item with fontsize = i}
	  | Tag (s,i) -> {item with tags = add_tag s i item.tags}
	  | Width f -> {item with width = f}
	  | Height f -> {item with height = f}
	  | Direction f -> {item with orientation = f}
	  | Shape s -> {item with shape = s}
	  | Radius f -> parse_attributes p s [Width f;Height f] item 
	  | Scale f -> {item with scale_factor = f *. item.scale_factor}
	  | Set_scale f -> {item with scale_factor = f}
    end 
    in parse_attributes p s q item 

let add_sig f item remanent = 
  let id = succ remanent.nsig_items in 
  let item = f id item in 
   id,
  {
    remanent 
   with 
     sig_items  = IdMap.add id item remanent.sig_items ; 
     nsig_items = id
  }

let p_txt x = 
  match x with 
  | Comment _ | Tag _ | Direction _  | Set_scale _ | Scale _ | Radius _ | Shape _ | FillColor _ -> false 
  | Color _ | Fontsize _  | Width _ | Height _ -> true 

let p_agent_type x =
  match x with 
  | Comment _ | Tag _ | Direction _ ->  false 
  | Set_scale _ | Scale _ | Radius _ | Shape _ | FillColor _ | Color _ | Fontsize _  | Width _ | Height _ -> true 
    
    
let p_site_type x = 
  match x with 
  | Comment _ | Tag _ -> false  
  | Set_scale _ | Scale _ | Radius _ | Shape _ | Direction _ | FillColor _ | Color _ | Fontsize _  | Width _ | Height _ -> true 

let p_state_type = p_site_type 

let p_agent x =
  match x with 
  | Comment _ | Direction _ ->  false 
  | Tag _ | Scale _ | Set_scale _ | Radius _ | Shape _ | FillColor _ | Color _ | Fontsize _  | Width _ | Height _ -> true 

let p_free x = 
  match x with 
  | Comment _ | Shape _ |  Fontsize _  ->  false 
  | Direction _ | Scale _ | Set_scale _ | Radius _  | Width _ | Height _ | Color _ | FillColor _ | Tag _  -> true

let p_bound = p_free 

let p_site  x = 
  match x with 
  | Comment _ -> false 
  | Tag _ | Set_scale _ | Scale _ | Radius _ | Shape _ | Direction _ | FillColor _ | Color _ | Fontsize _  | Width _ | Height _ -> true 

let p_state = p_site

let lift_agent (ag,_,_) = ag 
let lift_site (_,site,_) = site
let lift_state (_,_,state) = state 
let compose f g = (fun x -> f(g x))
let compose_lift (ag1,site1,state1) (ag2,site2,state2) = 
  (compose ag1 ag2,
   compose site1 site2,
   compose state1 state2)

let add_agent_type name attributes remanent = 
  let ag_c = remanent.nagent_sig_items +1 in 
  let remanent = {remanent with nagent_sig_items = ag_c} in 
  let f id item = {item with label = escaped name (Some item.fontsize)} in 
  let agent_sig = 
    parse_attributes p_agent_type "agent type" attributes {(dummy_agent_type remanent.config)  with fillcolor = n_modulo  (remanent.config.agent_colors) ag_c }
  in 
  add_sig f agent_sig remanent 

let add_sibbling s_id s_type map = 
  let old = 
    match 
      IdMap.find_option s_type map 
    with 
    | None -> IdSet.empty 
    | Some a -> a 
  in 
  IdMap.add s_type (IdSet.add s_id old) map

let add_son_type father error1 error2 error3 name dummy attributes p color_list remanent = 
  match IdMap.find_option father remanent.sig_items 
  with 
  | None -> 
    let _ = Printf.fprintf stderr "ERROR: in %s, try to add %s to an unknown %s\n" error1 error2 error3 
    in dummy_id,remanent
  | Some father_item -> 
    let sibbling_id = succ father_item.n_sibblings in 
    let father_item = { father_item with n_sibblings = sibbling_id } in 
    let color = n_modulo color_list sibbling_id in 
    let angle = angle_of_index sibbling_id in 
    let sibbling_item = 
      parse_attributes p error3 attributes {dummy with fillcolor = color ; orientation = angle}
    in 
    let sibbling_item = 
      {sibbling_item with father = Some father ; label = escaped name (Some sibbling_item.fontsize)}
    in 
    let s_id,remanent = add_sig (fun _ x -> x) sibbling_item remanent in 
    let father_item = 
      { father_item with sibblings = 
	  add_sibbling s_id s_id father_item.sibblings} 
    in 
    s_id,{remanent with sig_items = IdMap.add father father_item remanent.sig_items}

let add_site_type agent_type name attributes remanent = 
  add_son_type agent_type "add_site_type" "a site" "agent" name (dummy_site_name remanent.config) attributes p_site_type remanent.config.site_colors remanent 
      
let add_internal_state_type site_type state attributes remanent = add_son_type site_type "add_internal_state_type" "an internal state" "site" state (dummy_state_kind remanent.config) attributes p_state_type remanent.config.state_colors remanent 

let name_of_node id remanent = 
  let rec aux id list = 
    match  
      IdMap.find_option id remanent.items 
    with 
    | None -> 
      let _ = Printf.fprintf stderr "ERROR: dandling pointer in the data-structures\n"
	  in list
    | Some item -> 
      begin
	match item.father 
	with 
	| None -> (id,item.label)::list
	| Some id' -> aux id' ((id,item.label)::list)
      end
  in 
  let list = aux id [] in 
  let prefix,list  = 
    match  
	IdMap.find_option id remanent.items 
    with 
    | None -> 
      let _ = Printf.fprintf stderr "ERROR: dandling pointer in the data-structures\n" in "",list
    | Some x -> (string_of_node_kind x.kind),list
  in 
    List.fold_left
      (fun s (i,l) -> s^"_"^(string_of_int i))
      prefix list 
      
let add_node f item remanent = 
  let id = succ remanent.nitems in 
  let item = f id item in 
   id,
  {
    remanent 
   with 
     items  = IdMap.add id item remanent.items ; 
     nitems = id
  }

let add_agent agent_type abs ord attributes remanent = 
  match 
    IdMap.find_option agent_type remanent.sig_items 
  with 
  | None -> 
    let _ = Printf.fprintf stderr "ERROR: in add_agent, this agent type does not exist\n" in dummy_id,remanent 
  | Some item -> 
    let f id item = item in 
    let item = parse_attributes p_agent "agent" attributes item in 
    let item = {item with kind = Agent agent_type ; coordinate = {abscisse = abs ; ordinate = ord }; sibblings = IdMap.empty } in 
    let id,remanent = add_node f item remanent in 
    id,{remanent 
	with 
	  agents = 
	IdMap.add agent_type (match IdMap.find_option agent_type remanent.agents 
	  with None -> IdSet.singleton id
	  | Some l -> IdSet.add id l) remanent.agents}

let add_son father son_type kind error1 error2 error3 attributes p remanent = 
  match 
    IdMap.find_option son_type remanent.sig_items,
    IdMap.find_option father remanent.items
  with 
  | None,_ -> 
    let _ = Printf.fprintf stderr "ERROR: in %s, try to add %s to an unknown %s\n" error1 error2 error3 
    in dummy_id,remanent
  | _,None -> 
    let _ = Printf.fprintf stderr "ERROR: in %s, dandling pointer\n" error1 
    in dummy_id,remanent
  | Some item,Some father_item -> 
    let item = parse_attributes p error3 attributes {item with tags = father_item.tags ; orientation = match kind with State _ -> father_item.orientation | _ -> item.orientation  } in 
    let site_center = 
      (match father_item.shape 
      with 
      | "hexagone" -> point_on_hexagone
      | "rectangle" | "square" -> point_on_rectangle 
      | _ -> point_on_ellipse) 
	father_item.coordinate 
	(father_item.width *. father_item.scale_factor)
	(father_item.height *. father_item.scale_factor)
	item.orientation 
	item.scale_factor 
    in 
    let item = {item with father = Some father ; kind = kind ; label = item.label ; coordinate = site_center ; sibblings = IdMap.empty } in 
    let s_id,remanent = add_node (fun _ x -> x) item remanent in 
    let father_item = { father_item with sibblings = add_sibbling s_id son_type father_item.sibblings } in 
    s_id,{remanent with items = IdMap.add father father_item remanent.items}

let add_site agent name attributes remanent = 
  add_son agent name (Site name) "add_site" "a site" "agent" attributes p_site remanent 
      
let add_internal_state site_type state attributes remanent = 
  add_son site_type state (State site_type) "add_internal_state" "an internal state" "site" attributes p_state remanent

let op edge = 
  {edge with backward = edge.forward ; forward = edge.backward}
  
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
    
        
let rec add_link edge n1 n2 remanent = 
  if compare n1 n2 < 0 
  then 
    add_link (op edge) n2 n1 remanent 
  else 
    {remanent 
     with 
       edges = 
	let old = 
	  match 
	    Id2Map.find_option (n1,n2) remanent.edges 
	  with 
	  | None -> []
	  | Some l -> l 
	in  
	Id2Map.add (n1,n2) (edge::old) remanent.edges}

      
let add_relation relation = add_link relation 
let add_match_elt config = add_link (pairing config)
let add_proj_elt config = add_link (projection config)
let add_edge x y z = add_relation (link z.config) x y z 
let add_weak_flow_and_link x y z = add_relation (weak_flow z.config) x y (add_edge  x y z)
let add_flow_and_link x y z = add_relation (flow z.config) x y (add_weak_flow_and_link x y z)
let add_strong_flow_and_link x y z = add_relation (strong_flow z.config) x y (add_flow_and_link x y z)
  
let add_weak_flow x y z = add_relation (weak_flow z.config) x y z 
let add_flow x y z = add_relation (flow z.config) x y (add_weak_flow x y z)
let add_strong_flow x y z = add_relation (strong_flow z.config) x y (add_flow x y z)
  
let color_of_edge e = e.color
  
let pen_width_of edge = 
  if edge.width < 2. then "1" 
  else string_of_int (int_of_float edge.width)
    
let add_free site_id attributes remanent = 
  match 
    IdMap.find_option site_id remanent.items 
  with 
  | None -> 
    let _ = Printf.fprintf stderr "ERROR: in add_free the site does not exist\n" in 
    dummy_id,remanent 
  | Some site -> 
    let item = {dummy_item  with shape = "plaintext" ; father = Some site_id ; orientation = site.orientation ; width = remanent.config.free_width ; height = remanent.config.free_height ; tags = site.tags } in 
    let item = parse_attributes p_free "add_free" attributes item in 
    let free_center1 = point_on_ellipse_ext site.coordinate site.width site.height item.orientation site.scale_factor item.height in 
    let free_center2 = point_on_ellipse_ext free_center1 0. 0. (clockwise item.orientation 90.) 1. (item.width/.2.) in 
    let free_center3 = point_on_ellipse_ext free_center1 0. 0. (anticlockwise item.orientation 90.)  1. (item.width/.2.) in 
    let node = {item with width = 0. ; height = 0. ; color = "white" ; style = "" } in 
    let node_id1 = {node with coordinate = free_center1 ; kind = Free 1 ; sibblings = IdMap.empty } in 
     let id1,remanent = add_node (fun _ x -> x) node_id1  remanent in
     let id2,remanent = add_node (fun _ x -> x) {node with coordinate = free_center2 ; kind = Free 2 ; father = Some id1 } remanent in 
    let id3,remanent = add_node (fun _ x -> x) {node with coordinate = free_center3 ; kind = Free 3 ; father = Some id1 } remanent in 
    let remanent = {remanent with items = IdMap.add id1 {node_id1 with sibblings = add_sibbling id2 (-1) (add_sibbling id3 (-1) IdMap.empty)} remanent.items} in 
    let remanent = add_link {item with kind = Free_symbol 1} site_id id1 (add_link {item with kind = Free_symbol 2} id2 id3 remanent) in 
    id1,{remanent with items = IdMap.add site_id {site with sibblings = add_sibbling id1 (-1) (add_sibbling id2 (-1) (add_sibbling id3 (-1) site.sibblings))} remanent.items}

let add_bound site_id attributes remanent = 
  match 
    IdMap.find_option site_id remanent.items 
  with 
  | None -> 
    let _ = Printf.fprintf stderr "ERROR: in add_bound, the site does not exist\n" in 
    dummy_id,remanent 
  | Some site -> 
    let item = {dummy_item  with shape = "plaintext" ; father = Some site_id ; orientation = site.orientation ; height = remanent.config.bound_height ; kind = Bound ; fillcolor = "black" ; tags = site.tags } in 
    let item = parse_attributes p_bound "add_bound" attributes item in 
    let fillcolor = item.fillcolor in 
    let color = item.color in 
    let bound_center = point_on_ellipse_ext site.coordinate site.width site.height item.orientation item.scale_factor item.height in 
    let item = {item with height = 0. ; color = "white" ; fillcolor = "white" } in 
    let id1,remanent = add_node (fun _ x -> x) {item with coordinate = bound_center ; kind = Bound} remanent in 
    let remanent = add_link {item with kind = Bound_symbol ; width = 0. ; height = 0. ; fillcolor = fillcolor ; color = color}  site_id id1 remanent in 
    id1,{remanent with items = IdMap.add site_id {site with sibblings = add_sibbling id1 (-1) site.sibblings} remanent.items}
    
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
    let rec aux todo = 
      match todo
      with 
      | [] -> true
      | x::q -> 
	let s = s_of_x x in 
	match TagMap.find_option s map 
	with None -> deft && aux q 
	| Some set ->
	  if p x set 
	  then aux q 
	  else false
    in aux l 
  in 
  def list true (fun (s,i) set -> IntSet.mem i set) fst 
    
let dump_edge log (s1,s2) edge remanent = 
  Printf.fprintf log "%s -> %s [dir = \"%s\",color=\"%s\",penwidth=%s,label=\"%s\",style=\"%s\"];\n" s1 s2 (dir_of_edge edge) (color_of_edge edge) (pen_width_of edge) (edge.comment)  (edge.style)
    
    
let dump_node log filter filter_label filter_color node remanent = 
  Printf.fprintf log 
    "fixedsize=true,\nlabel = \"%s\",\nfontsize=%i,\npos=\"%g,%g!\",\nwidth=%f,\nheight=%f,\nshape=\"%s\",\nstyle=\"%s\",\nfillcolor=%s,\ncolor=%s\n" (filter_label node.label) node.fontsize node.coordinate.abscisse node.coordinate.ordinate (node.width*.node.scale_factor)  (node.height*.node.scale_factor)  node.shape node.style (filter_color node.fillcolor)  node.color
    
let dump_node chan filter remanent node_id node =
  let tags = node.tags in 
  let filter_label,filter_color = 
    match 
      node.kind 
    with 
    | Agent _ -> filter_label_in_agent,filter_color_in_agent
    | Site _ -> filter_label_in_site,filter_color_in_site
    | State _ -> filter_label_in_state, filter_color_in_state 
    | _ -> (fun _ x ->x),(fun _ x ->x)
  in 
  if filter_tags filter tags 
  then 
    let _ = Printf.fprintf chan "%s [\n" (name_of_node node_id remanent) in 
    let _ = dump_node chan filter (filter_label remanent) (filter_color remanent) node remanent in 
    let _ = Printf.fprintf chan "]\n\n" in 
    ()
      
let dump_edge_list chan filter remanent (n1,n2) l = 
  match 
    IdMap.find_option n1 remanent.items,
    IdMap.find_option n2 remanent.items
  with
  | None,_ | _,None -> Printf.fprintf stderr "ERROR: in dump_edge_list, dandling pointers\n" 
  | Some node1,Some node2 -> 
    let l = List.filter 
      (fun edge -> 
	(filter_tags filter node1.tags && 
		      filter_tags filter node2.tags && 
		      filter_tags filter edge.tags )) l
    in 
    let n1 = name_of_node n1 remanent in 
    let n2 = name_of_node n2 remanent in 
    let n1,n2 = 
      if node1 == node2 
      then 
	let angle = node1.orientation.radius in 
	let suffix = 
	  if angle < pi/.4. then ":n"
	  else if angle < 3.*.pi/.4. then ":e"
	  else if angle < 5.*.pi/.4. then ":s"
	  else if angle < 7.*.pi/.4. then ":w"
	  else ":n"
	in 
	n1^suffix,n2^suffix
      else
	n1,n2
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
	  then 
	    aux q accu 
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
    | Some edge -> 
      dump_edge chan (n1,n2) edge remanent 
      

let dump file filter remanent = 
  let chan = open_out file in 
  let _ = Printf.fprintf chan "digraph G {\n\n" in 
  let _ = 
    IdMap.iter
      (dump_node chan filter remanent)
      remanent.items
  in 
  let _ = 
    Id2Map.iter 
      (dump_edge_list chan filter remanent)
      remanent.edges 
  in 
  let _ = Printf.fprintf chan "}\n" in 
  let _ = close_out chan in 
  ()
    
let new_internal_state_type agent_type site_type state d (remanent,state_list) = 
  let id,remanent = add_internal_state_type site_type state d remanent in 
  remanent,(id::state_list)
    
let new_site_type agent_type site_type state_list d (remanent,site_list) = 
  let id,remanent = add_site_type agent_type site_type d remanent in 
  let remanent,list = 
    List.fold_left 
      (fun (remanent,list) (state,d) -> 
	new_internal_state_type agent_type id state d (remanent,list)) (remanent,[]) state_list 
  in 
  remanent,(id,List.rev list)::site_list
    
let new_agent_type agent_type site_list d (remanent,agent_list) = 
  let id,remanent = add_agent_type agent_type d remanent in 
  let remanent,list = 
    List.fold_left 
      (fun (remanent,list) (site,d,state_list ) -> 
	new_site_type id site state_list d (remanent,list)) 
      (remanent,[]) site_list 
  in 
  remanent,(id,List.rev list)::agent_list
    
let add_in_signature remanent (signature:signature) = 
  let remanent,list = 
    List.fold_left 
      (fun (remanent,list) (agent,d,site_list) -> 
	new_agent_type agent site_list d (remanent,list))
      (remanent,[]) signature 
  in remanent,List.rev list
    
let new_state agent site (remanent,state_list) state =       
  let id,remanent = 
    match state
    with 
    | Free_site op  -> add_free site op remanent
    | Bound_site op -> add_bound site op remanent 
    | Internal_state (op1,op2) -> add_internal_state site op1 op2  remanent 
  in 
  remanent,id::state_list 
    
let new_site agent (remanent,site_list) (site,directives,states) = 
  let site,remanent = add_site agent site  directives remanent in 
  let remanent,states  = 
    List.fold_left 
      (new_state agent site) 
      (remanent,[]) 
      states 
  in 
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
    
    
let fold_id_and_nodes f remanent = IdMap.fold f remanent.items
let fold_id f = fold_id_and_nodes (fun x _ -> f x)
let fold_node f = fold_id_and_nodes (fun _ y -> f y)
  
let map_node f1 f2 remanent = 
  {remanent with 
    items = IdMap.map f1  remanent.items ;
    edges = Id2Map.map (fun x -> List.rev_map f2 (List.rev x)) remanent.edges 
  }

let apply_opt f opt = 
  match 
    opt
  with 
  | None -> None
  | Some opt -> Some (f opt)

let apply_f_id_in_item f_id item = 
  { item 
    with 
      father = apply_opt f_id item.father ;
      sibblings = 
      IdMap.map (fun x -> IdSet.fold (fun x -> IdSet.add (f_id x)) x IdSet.empty ) item.sibblings
  }
  
let map_id f_id remanent = 
  let items,nitems = 
    IdMap.fold
      (fun id item (map,nitems) -> 
	IdMap.add (f_id id) (apply_f_id_in_item f_id item) map,
 	max (f_id id) nitems)
      remanent.items
      (IdMap.empty,0) 
  in 
  let nitems = max (f_id remanent.nitems) (nitems+1) in 
  let edges = 
    Id2Map.fold 
      (fun (id1,id2) edge map -> 
	let id1,id2 = f_id id1,f_id id2 in 
	let id1,id2,f = 
	  if compare id1 id2 < 0 
	  then 
	    id2,id1,op
	  else 
	    id1,id2,(fun x->x)
	in 
	Id2Map.add 
	  (id1,id2)
	  (List.rev_map 
	     (fun item -> f (apply_f_id_in_item f_id item))
	     (List.rev edge)) 
	  map)
      remanent.edges 
      Id2Map.empty
  in 
  let agents = IdMap.map (fun y -> IdSet.fold (fun x -> IdSet.add (f_id x)) y IdSet.empty) remanent.agents
  in 
  let remanent = 
    {remanent with 
      agents = agents;
      edges = edges ;
      items = items ;
      nitems = nitems }
  in 
  f_id,f_id,f_id,remanent 
    
let add_to_id n remanent = 
  map_id (fun x -> x+n) remanent 
    
    
let max_id remanent = fold_id max remanent 0 
let corners remanent = 
  let fusion (x,y,z,t) (x',y',z',t') = (min x x',max y y',min z z',max t t') 
  in 
  fold_node 
    (fun node pos ->
      let x,y=node.coordinate.abscisse,node.coordinate.ordinate in 
      let coords =
	x-.node.width/.2.,x+.node.width/.2.,y-.node.height/.2.,y+.node.height/.2.
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
let translate_graph vector remanent = 
  map_node 
    (fun node -> 
      {node with 
	coordinate = translate node.coordinate vector ;
      })
     (fun node -> 
      {node with 
	coordinate = translate node.coordinate vector ;
      })
    remanent 
    
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
	{node with 
	  coordinate = 
	    {node.coordinate 
	     with ordinate = 2.*.axe -. node.coordinate.ordinate};
	  orientation = of_degree (180. -. (to_degree node.orientation))})
       (fun node -> 
	{node with 
	  coordinate = 
	    {node.coordinate 
	     with ordinate = 2.*.axe -. node.coordinate.ordinate};
	  orientation = of_degree (180. -. (to_degree node.orientation))})
      remanent 
let vertical_swap remanent = 
  match corners remanent 
  with 
  | None -> remanent 
  | Some (x_min,x_max,_,_) -> 
    let axe = (x_max +. x_min) /. 2. in 
    map_node 
      (fun node -> 
	{node with 
	  coordinate = 
	    {node.coordinate 
	     with abscisse = 2.*.axe -. node.coordinate.abscisse};
	  orientation = of_degree (360. -. (to_degree node.orientation))})
        (fun node -> 
	{node with 
	  coordinate = 
	    {node.coordinate 
	     with abscisse = 2.*.axe -. node.coordinate.abscisse};
	  orientation = of_degree (360. -. (to_degree node.orientation))})
      remanent 
      
let fuse fold2map addmap f map1 map2 = 
  fold2map
    (fun x y z map -> f x y z map)
    (fun x y map -> map)
    (fun x z map -> addmap x z map)
    map1 
    map2
    map1
    
    (* carefully review the following function and think, I think the precondistion is to restrictive *)
    (* test with two graphs with distinct interfaces on a agent, *)
    (* test with two graphs with distinct states on a site*)
let fuse remanent remanent' = 
  let _ = 
    if not (remanent.config == remanent'.config)
    then Printf.fprintf stderr "Warning: In fuse, graphs have a different configurations settings.\n"
  in 
  let _ = 
    if not (remanent.sig_items == remanent'.sig_items)
    then Printf.fprintf stderr "Warning: In fuse, graphs should have the same signature.\n" 
  in 
  let nitems = max remanent.nitems remanent'.nitems in 
  let items = 
    fuse IdMap.fold2 IdMap.add 
      (fun x y z map -> 
	if y==z then map 
	else if assert_compatible_items y z 
	then IdMap.add x { y with sibblings = 
	    IdMap.fold2 
	      (fun x y z map -> 
		IdMap.add x (IdSet.union y z) map)
	      (fun x y map -> map)
	      (fun x y map -> IdMap.add x y map)
	      y.sibblings
	      z.sibblings
	      y.sibblings
			 }
	  map
	else 
	  (let _ = Printf.fprintf stderr "ERROR: In fuse, try to fuse maps with different images (items)\n" in map))

      remanent.items remanent'.items 
  in 
  let agents  = 
    fuse IdMap.fold2 IdMap.add 
      (fun x y z map -> 
	if y==z then map 
	else IdMap.add x (IdSet.union y z) map)
      remanent.agents remanent'.agents
  in 
  let edges = 
    fuse Id2Map.fold2 Id2Map.add 
      (fun x y z map -> if y==z then map 
	else let _ = Printf.fprintf stderr "ERROR: in fuse, incompatible edges\n" in map) remanent.edges remanent'.edges 
  in 
  { 
    remanent 
    with 
      agents = agents ;
      nitems = nitems ; 
      edges=edges ;
      items = items ;
  }
    
let disjoint_union remanent remanent' = 
  let delta = remanent.nitems in 
  let (f,g,h,remanent') = add_to_id delta remanent' in 
  ((fun x->x),
  (fun y->y),
  (fun z->z)),
  (f,g,h),
  fuse remanent remanent'
    
    
let add_match l remanent =  
  List.fold_left 
    (fun remanent (x,y) -> add_match_elt remanent.config x y remanent) remanent l 
    
    
let add_proj l remanent = 
  List.fold_left 
    (fun remanent (x,y) -> add_proj_elt remanent.config x y remanent) remanent l 
    
let add_emb list remanent = remanent
let disjoint_union_with_match remanent remanent = 
  ((fun x -> x),(fun x -> x),(fun x -> x)),
  ((fun x->x),(fun x->x),(fun x->x)),remanent 
    
let move_remanent_right_to delta remanent remanent' = 
  match corners remanent,corners remanent'
  with None, _ -> remanent
  | _,None -> remanent
  | Some (xmin,xmax,ymin,ymax),Some (xmin',xmax',ymin',ymax') -> 
    translate_graph {abscisse = xmax'-.xmin+.delta ; ordinate =  0.} remanent 
      
let move_remanent_left_to delta remanent remanent' = 
  match corners remanent,corners remanent'
  with None, _ -> remanent
  | _,None -> remanent
  | Some (xmin,xmax,ymin,ymax),Some (xmin',xmax',ymin',ymax') -> 
    translate_graph {abscisse = xmin'-.xmax-.delta ; ordinate = 0.} remanent 
      
      
let move_remanent_above delta remanent remanent' = 
  match corners remanent,corners remanent'
  with None, _ -> remanent
  | _,None -> remanent
  | Some (xmin,xmax,ymin,ymax),Some (xmin',xmax',ymin',ymax') -> 
    translate_graph { abscisse = 0.; ordinate =  ymax'-.ymin+.delta} remanent 
      
let move_remanent_bellow delta remanent remanent' = 
  match corners remanent,corners remanent'
  with None, _ -> remanent
  | _,None -> remanent
  | Some (xmin,xmax,ymin,ymax),Some (xmin',xmax',ymin',ymax') -> 
    translate_graph { abscisse = 0. ; ordinate = ymin'-.ymax-.delta} remanent 
      
let init config = 
  let remanent = init config in 
  add_agent_type 
    "" 
    [Width (0.);Height(0.);FillColor("white");Color("white")]
    remanent
    
let dummy_node_type = 1 
  
let filter_out_direction = 
  List.filter
    (fun x -> match x with Direction _ -> false | _ -> true)
    
let add_dummy_agent string x y directives remanent = 
  let f id item = item in 
  let item = parse_attributes (fun _ -> true) "" directives { dummy_item with coordinate = {abscisse = x ; ordinate = y}} in 
  add_node f {item with width = 0. ; height = 0. ; color = "white" ; style = ""} remanent 
       
let add_rule x y directives remanent = 
  let item = {dummy_item with coordinate = {abscisse = x ; ordinate = y} ; width = float_of_int (remanent.config.rule_width) ; height = remanent.config.rule_length ; orientation = e} in 
  let item = parse_attributes (fun _ -> true) "" directives item  in  
  let size = item.width in 
  let angle = item.orientation.radius in 
  let x = item.coordinate.abscisse in 
  let y = item.coordinate.ordinate in 
  let x1 = x +. size *. (sin angle) /. 2. in 
  let y1 = y +. size *. (cos angle) /. 2. in 
  let x2 = x -. size *. (sin angle) /. 2. in 
  let y2 = y -. size *. (cos angle) /. 2. in 
  let node_id1,remanent = add_dummy_agent "add_rule" x1 y1 directives remanent in 
  let node_id2,remanent = add_dummy_agent "add_rule" x2 y2 directives remanent in 
  match 
    IdMap.find_option node_id1 remanent.items,
    IdMap.find_option node_id2 remanent.items
  with 
  | None,_ | _,None -> 
    let _ = Printf.fprintf stderr "ERROR, cannot create a rule, the just created nodes do not exist\n" in 
    remanent 
  | Some node1,Some node2
    -> 
    add_link 
      {(rule remanent.config) 
       with width = item.width ; comment = item.comment ;
	color = item.color ; tags = item.tags } node_id2 node_id1 remanent 
      
let cross remanent = 
  let directives = [] in 
  let comment = "" in 
  let width = float_of_int remanent.config.cross_width in 
  match corners remanent 
  with 
  | None -> remanent 
  | Some (x,x',y,y') ->
    let node_id1,remanent = 
      add_dummy_agent "cross" x y' directives remanent 
    in 
    let node_id2,remanent = 
      add_dummy_agent "cross" x' y  directives remanent
    in 
    let node_id3,remanent = 
      add_dummy_agent "cross" x y directives remanent 
    in 
    let node_id4,remanent = 
      add_dummy_agent "cross" x' y' directives remanent
    in 
    let edge = 
      {(rule remanent.config) 
       with 
	    forward = false ; width = width ; comment = comment ;
	    color = "red" ; tags = TagMap.empty }
    in 
    match 
      List.map 
	(fun x -> IdMap.find_option x remanent.items) 
	[node_id1;node_id2;node_id3;node_id4]
    with 
      [Some node1;Some node2;Some node3;Some node4] -> 
	add_link edge node_id4 node_id3 (add_link edge node_id2 node_id1 remanent )
    | _ -> 
      let _ = Printf.fprintf stderr "ERROR, cannot create a cross, the just created nodes do not exist\n" in remanent 
													  
													  
let tag_all_nodes t i remanent = 
  map_node 
      (fun node -> 
	{node with tags = add_tag t i node.tags})
      (fun x -> x)
      remanent 
  
let lift_list2 fst snd l = 
  List.rev_map 
    (fun (s,l) -> (fst s,snd l))
    (List.rev l)
let lift_state_list sigma l = 
  List.rev_map 
    (fun x -> lift_state sigma x)
    (List.rev l)
let lift_site_list sigma = 
  lift_list2 (lift_site sigma) (lift_state_list sigma) 
let lift_agent_list sigma = 
  lift_list2 (lift_agent sigma) (lift_site_list sigma)

let build_rule domain extend_lhs extend_rhs directives = 
  let node = {dummy_item with orientation = e ; width = float_of_int (domain.config.rule_width)} in 
  let node = parse_attributes (fun _ -> true) "" directives node in 
  let angle = sample_angle node.orientation in 
  let (l1,l2,l3),lhs = extend_lhs domain in 
  let (r1,r2,r3),rhs = extend_rhs domain in 
  let lhs = tag_all_nodes "lhs" 1 lhs in 
  let rhs = tag_all_nodes "rhs" 1 rhs in 
  let cornerlhs = 
    match 
      corners lhs
    with 
      Some (xm,xM,ym,yM) -> xm,xM,ym,yM 
    | None -> 0.,0.,0.,0.
  in 
  let cornerrhs = 
    match 
      corners rhs
    with 
      Some (xm,xM,ym,yM) -> xm,xM,ym,yM 
    | None -> 0.,0.,0.,0.
  in 
  let distance = (1.+.domain.config.rule_margin*.2.)*. node.width in
  let rulex,ruley,deltax,deltay = compute_padding cornerlhs cornerrhs angle distance in 
  let sigmal,sigmar,rule = disjoint_union lhs (translate_graph {abscisse=deltax;ordinate=deltay} rhs) in 
  let rule = add_rule rulex ruley directives rule in 
  sigmal,
  sigmar,
  (List.concat [lift_agent_list sigmal l1;lift_agent_list sigmar r1],
  List.concat [lift_site_list sigmal l2;lift_site_list sigmar r2],
  List.concat [lift_state_list sigmal l3;lift_state_list sigmar r3]),
   rule 

let rename_tag s s' = 
  let f = 
    (fun n -> 
      {n with tags = 
	  TagMap.fold 
	    (fun tag i map -> 
	      if tag = s 
	      then 
		let old = 
		  match  
		    TagMap.find_option s' map
		  with 
		  | Some x -> x 
		  | None -> IntSet.empty 
		in 
		TagMap.add s' (IntSet.union old i) 
		  (TagMap.remove s map)
	      else 
		map )
	    n.tags
	    n.tags})
  in map_node f f 

let tag_flow_from graph1 ag s s' graph2 = 
  let edge2 = 
    Id2Map.fold 
      (fun (id1,id2) (edge:'a list) edge2 -> 
	match 
	    IdMap.find_option id1 ag
	  with 
	  | None -> edge2 
	  | Some id1_list' -> 
	    begin
	      match 
		IdMap.find_option id2 ag 
	      with 
	      | None -> edge2 
	      | Some id2_list' -> 
		begin
		  List.fold_left 
		    (fun edge2 edge_elt -> 
		      match TagMap.find_option s edge_elt.tags 
		      with 
		      | None -> edge2 
		      | Some i -> 
			begin 
			  List.fold_left 
			    (fun edge2 id1' -> 
			      List.fold_left 
				(fun edge2 id2' -> 
				  let id1',id2',edge_elt = 
				      if compare id1' id2' < 0 
				      then id2',id1',op edge_elt
				      else id1',id2',edge_elt
				  in
				  let old_l = 
				    match 
				      Id2Map.find_option (id1',id2') edge2 
				    with 
				    | None -> []
				    | Some l -> l 
				  in 
				    Id2Map.add (id1',id2') 
				      ({edge_elt with tags = TagMap.add s' i (TagMap.remove s edge_elt.tags)}::old_l) edge2)
				edge2 id2_list')
			    edge2 id1_list'
			end)
		    edge2 
		    (List.rev edge )
	      end 
	  end )
      graph1.edges 
      graph2.edges
  in 
  {graph2 with edges = edge2}

let add_asso x y map = 
  let l = 
    match 
      IdMap.find_option x map 
    with 
    | None -> []
    | Some l -> l
  in 
  IdMap.add x ((y:id)::l) map

let site_map_from_agent_list graph1 graph2 l = 
  List.fold_left 
    (fun map (x,y) -> 
      match 
	IdMap.find_option x graph1.items
      with 
      | None -> map
      | Some x -> 
	begin 
    	  match 
	    IdMap.find_option y graph2.items
	  with 
	  | None -> map
	  | Some y -> 
    	    let l_sites = x.sibblings in 
	    IdMap.fold 
	      (fun s_type -> 
		IdSet.fold 
		  ( fun s_id map -> 
		    let l_sites2 = 
		      match 
			IdMap.find_option s_type y.sibblings 
		      with 
		      | None -> IdSet.empty
		      | Some l -> l 
		    in 
		    IdSet.fold (fun a b -> add_asso s_id a b)
		      l_sites2 map))
	      l_sites map
	end
    )
    IdMap.empty l 


let proj_flow_on_a_species ?file:(s="") ?padding:(padding=1.) ?angle:(angle = e)  ?flow:(flow=[]) rule species l =
  let rule = add_flow_list flow rule in 
  let xm,xM,ym,yM = 
    match 
      corners rule 
    with 
      None -> 0.,0.,0.,0.
    | Some (xm,xM,ym,yM) -> xm,xM,ym,yM 
  in 
  let xm',xM',ym',yM' = 
    match 
      corners species 
    with 
      None -> 0.,0.,0.,0.
    | Some (xm,xM,ym,yM) -> xm,xM,ym,yM 
  in 
  let angle = sample_angle angle in 
  let padding = rule.config.flow_padding *. padding in 
  let distance = padding*. ((xM-.xm)*.(sin (to_radius angle)) +. (yM-.ym)*.(cos (to_radius angle))) in 
  let rulex,ruley,deltax,deltay = compute_padding (xm,xM,ym,yM) (xm',xM',ym',yM') angle distance in 
  let rule = rename_tag "flow" "rule_flow" rule in 
  let agent_map = 
    List.fold_left (fun map (x,y) -> IdMap.add x y map) IdMap.empty l in 
  let site_map = site_map_from_agent_list rule species l in 
  let species = tag_flow_from rule site_map "rule_flow" "sp_flow" species in 
  let sigma_rule,sigma_sp,rule_species = disjoint_union rule (translate_graph {abscisse=deltax;ordinate=deltay} species) in 
  let rule_species_with_proj = 
    add_proj
      (List.rev_map (fun (x,y) -> lift_agent sigma_rule x,lift_agent sigma_sp y) l) rule_species 
  in 
  let _ = 
    match s with 
    | "" -> ()
    | _  -> 
      let _ = dump (s^"0.dot") ["rule_flow",0;"sp_flow",0] rule_species in 
      let _ = dump (s^"1.dot") ["rule_flow",1;"sp_flow",0] rule_species in 
      let _ = dump (s^"2.dot") ["rule_flow",1;"sp_flow",0] rule_species_with_proj in 
      let _ = dump (s^"3.dot") ["rule_flow",1;"sp_flow",1] rule_species_with_proj in 
      let _ = dump (s^"4.dot") ["rule_flow",1;"sp_flow",1] rule_species in 
      () 
  in sigma_rule,sigma_sp,rule_species,rule_species_with_proj

let proj_flow_on_a_contact_map ?file:(s="") ?padding:(padding=1.) ?angle:(angle = e)  ?flow:(flow=[]) rule contact_map =
  let agent_map = 
    IdMap.fold2z
      (fun x y z map -> 
	IdSet.fold 
	  (fun x map  -> 
	    IdSet.fold 
	      (fun y map  -> (x,y)::map)
	      z 
	      map
	  ) 
	  y 
	  map 
      )
      (fun x y map -> map)
      (fun x y map -> map)
      rule.agents
      contact_map.agents 
      []
  in 
  proj_flow_on_a_species ~file:s ~angle:angle 
    ~padding:padding 
    ~flow:flow 
    rule 
    contact_map
    agent_map 

let insert_text_here s x y d remanent = 
   let item = parse_attributes p_txt "txt" d (dummy_txt_item remanent.config) in 
   let item = 
    {item 
     with label = escaped s (Some item.fontsize) ; 
    coordinate = {abscisse = x ; ordinate = y}}
  in 
  let f _ x = x in 
  let id,remanent = add_node f item remanent in 
  remanent

let insert_text_on_boarder list ?inside:(x=Outside) d  graph = 
  match corners graph 
  with 
  | None -> 
	List.fold_left 
	  (fun graph (s,_) -> insert_text_here s 0. 0. d graph)
	  graph list 
  | Some (x_min,x_max,y_min,y_max) -> 
    let x = (x_min+.x_max)/.2. in 
    let y = (y_min+.y_max)/.2. in 
    let dx = x_max -. x_min in 
    let dy = y_max -. y_min in 
    List.fold_left 
      (fun remanent (s,direction) -> 
	let direction = Geometry.correct_angle_on_rect dx dy direction in 
	let center = 
	  point_on_rectangle 
	    {abscisse=x;ordinate=y}
	    dx
	    dy 
	    direction
	    1.
	in 
	let _ = Printf.fprintf stdout "%f %f %f \n" dx dy (Geometry.to_degree direction) in 
	insert_text_here s center.abscisse center.ordinate d remanent )
      graph list 
