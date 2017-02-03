(**
 * gkapa.ml
 * GKappa
 * Jérôme Feret, projet Antique, INRIA Paris-Rocquencourt
 *
 * Creation:                      <2015-03-28 feret>
 * Last modification: Time-stamp: <2015-07-15 16:52:55 feret>
 * *
 *
 * Copyright 2015 Institut National de Recherche en Informatique  * et en Automatique.  All rights reserved.
 * This file is distributed under the terms of the
 * GNU Library General Public License *)


(* GKappa is a OCAML library to help making slides in Kappa *)
(* More applications are coming soon (hopefully) *)


open Geometry

type in_out = Inside | Outside
type ru_corner =
  {
    d_l : string ;
    d_r : string ;
    l_d : string ;
    r_d : string
  }
type lo_corner =
  {
    b'_b:string;
    b'_l:string;
    b'_r:string;
    b_b':string;
    b_l:string;
    b_r:string;
    l_b':string;
    l_b:string;
    l_t:string;
    l_t':string;
    r_b':string;
    r_b:string;
    r_t:string;
    r_t':string;
    t_l:string;
    t_r:string;
    t_t':string;
    t'_l:string;
    t'_r:string;
    t'_t:string;
  }

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
                 "$\\fontsize{"^(string_of_int i)^"}{"^(string_of_int (int_of_float(floor  (float_of_int i*.1.2))))^"}\\selectfont \\ladottext{"^s^"}$"
               else s

let escape_short a b =
  let sizea,sizeb = String.length a,String.length b in
  if sizea =0 || sizeb = 0
  then a,b
  else
  if String.sub a (sizea-1) 1 = "$" && String.sub b 0 1 = "$"
  then
    String.sub a 0 (sizea-1),String.sub b 1 (sizeb-1)
  else
    a,b

let escape_color a c =
  let sizea = String.length a in
  if sizea > 1 then
    if String.sub a 0 1  = "$" && String.sub a (sizea-1) 1 = "$"
    then
      let sizec = String.length c in
      let n = 20 + sizec in
      let size = (float_of_int (n*31))/.100. in
      "$\color{"^c^"}\hspace*{"^(string_of_float size)^"cm}"^(String.sub a 1 (sizea -1))
    else
      a
  else a

(*%module GKappa =
  %  struct
*)
type tag = string
type directive =
  | Fontsize of int
  | FontColor of string
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

type head_type = Normal | Vee
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
let lift_of_agent_map f = f,(fun x -> x),(fun x -> x)
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
    empty_graph: string;
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
    head_scale: float;
    head_type: head_type;
    tail_type: head_type;
    pairing_width: int;
    projection_width: int;
    cross_width: int;
    rule_name_font: int;
    txt_font: int;
    binding_type_font: int;
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
    losange:  (string*string)*(string*(string*string))*(string*(string*string))*(string*(string*string))*(string*(string*string))*(string*string);
    losange_corners:lo_corner;
    losange_padding: float;
    rule: (string*string)*(string*string);
    rule_corners:ru_corner;
  }


type co_mode = Empty | Middle | Corners

let empty_ru =
  {
    d_l = "" ;
    d_r = "" ;
    l_d = "" ;
    r_d = ""
  }
let middle_ru =
  {
    d_l = "" ;
    d_r = "" ;
    l_d = "s" ;
    r_d = "s"
  }
let corners_ru =
  {
    d_l = "ne" ;
    d_r = "nw" ;
    l_d = "se" ;
    r_d = "sw" ;
  }

let empty_co
  =
  {
    b'_b = "" ;
    b'_l = "" ;
    b'_r = "" ;
    b_b' = "" ;
    b_l = "" ;
    b_r = "" ;
    l_b' = "" ;
    l_b = "" ;
    l_t = "" ;
    l_t' = "" ;
    r_b' = "" ;
    r_b = "" ;
    r_t = "" ;
    r_t' = "" ;
    t_l = "" ;
    t_r = "" ;
    t_t' = "" ;
    t'_l = "" ;
    t'_r = "" ;
    t'_t = "" ;
  }

let middle_co
  =
  {
    b'_b = "n" ;
    b'_l = "n" ;
    b'_r = "n" ;
    b_b' = "s" ;
    b_l = "n" ;
    b_r = "n" ;
    l_b' = "s" ;
    l_b = "s" ;
    l_t = "n" ;
    l_t' = "n" ;
    r_b' = "s" ;
    r_b = "s" ;
    r_t = "n" ;
    r_t' = "n" ;
    t_l = "s" ;
    t_r = "s" ;
    t_t' = "n" ;
    t'_l = "s" ;
    t'_r = "s" ;
    t'_t = "s" ;
  }

let corners_co
  =
  {
    b'_b = "n" ;
    b'_l = "nw" ;
    b'_r = "ne" ;
    b_b' = "s" ;
    b_l = "nw" ;
    b_r = "ne" ;
    l_b' = "sw" ;
    l_b = "sw" ;
    l_t = "nw" ;
    l_t' = "nw" ;
    r_b' = "se" ;
    r_b = "se" ;
    r_t = "ne" ;
    r_t' = "ne" ;
    t_l = "sw" ;
    t_r = "se" ;
    t_t' = "n" ;
    t'_l = "sw" ;
    t'_r = "se" ;
    t'_t = "s" ;
  }



type intset = IntSet.t
type sig_kind = Agent_type | Site_type | State_type
type node_kind = Empty_agent | Agent of id | Site of id | State of id | Rule_source | Rule_target | Free of int | Bound | Dummy_node
type edge_kind =
  | Link
  | Relation of (node_kind * node_kind * head_type * (float option))
  | Free_symbol of int | Bound_symbol  | Rule | Dummy_edge

let assert_compatible_items x y = true (*to do*)

let string_of_node_kind x =
  match x
  with
  | Agent _ -> "Agent"
  | Site _ -> "Site"
  | State _ -> "State"
  | Rule_source -> "RuleS"
  | Rule_target -> "RuleT"
  | Dummy_node -> "Dummy_node"
  | Free i -> "Free"^(string_of_int i)
  | Bound -> "Bound"
  | Empty_agent -> "Empty"

let string_of_edge_kind x =
  match x
  with
  | Free_symbol i -> ("-|"^(string_of_int i))
  | Bound_symbol -> "-"
  | Dummy_edge -> "Dummy_edge"
  | Link -> "Link"
  | Relation (_,_,_,_) -> "Relation"
  | Rule -> "Rule"

let string_of_sig_type x =
  match x
  with
  | Agent_type -> "Agent_type"
  | Site_type -> "Site type"
  | State_type -> "State type"

type 'a item =
  {
    short_label: string;
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
    fontcolor:string ;
    color:string;
    coordinate: point;
    orientation: angle;
    scale_factor: float;
    corner1:string option;
    corner2:string option;
    canbefused:bool;
    h_type: head_type;
    t_type: head_type;
    h_scale: float;
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
    corner1=None;
    corner2=None;
    canbefused=true;
    kind = Dummy_node ;
    father = None ;
    sibblings = IdMap.empty ;
    label = "" ;
    short_label = "";
    tags = TagMap.empty;
    n_sibblings=0;
    width = 0.;
    height = 0.;
    fontsize = 0;
    fillcolor = "white";
    fontcolor = "black" ;
    color = "black";
    coordinate = origin;
    shape = "ellipse";
    scale_factor=1.;
    h_type= Normal ;
    t_type = Normal ;
    h_scale = 1. ;
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
         width = (float_of_int config.link_width);
         t_type = config.tail_type ;
         h_type = config.head_type ;
         h_scale = config.head_scale ;
  }
let pairing config =
  {
    (link config)
    with style = config.pairing_style ;
         color = config.pairing_color ;
         width = (float_of_int config.pairing_width)}
let projection ?name:(name=None) ?ca:(ca=None) ?cb:(cb=None) ?(donotfuse=false) config =
  {
    (pairing config)
    with
      forward = true ;
      comment =
        begin
          match
            name
          with
          | None -> ""
          | Some x -> x
        end;
      corner1 = ca ;
      corner2 = cb ;
      canbefused = not donotfuse ;
      style = config.projection_style ;
      color = config.projection_color ;
      width = (float_of_int config.projection_width)}
let rule config =
  { (link config)
    with forward = true ;
         kind = Rule ;
         width = (float_of_int config.rule_width) ;
         color = config.rule_color ; style = config.rule_style }

let update_head ?directives config =
  match
    directives
  with
  | None -> config
  | Some l ->
    List.fold_left
      (fun config a ->
         match a with
         | Color s ->
           { config
             with weak_flow_color = s}
         | Set_scale f ->
           { config
             with head_scale = f }
         | Scale f ->
           {config with head_scale = f*.config.head_scale}
         | Shape x ->
           begin
             match String.lowercase x
           with
             "normal" ->
             { config with head_type = Normal}
           | "vee" ->
             {config with head_type = Vee}
           | _ -> config
           end
         | _ -> config
      )
      config l



let weak_flow ?directives config =
  let config = update_head ?directives config in
  { (link config)
    with
      priority = 2;
      forward = true;
      color = config.weak_flow_color;
      style = config.weak_flow_style ;
      width = (float_of_int config.weak_flow_width);
      tags = TagMap.add "flow" (IntSet.add 1 (IntSet.add 2 IntSet.empty)) TagMap.empty}


let flow ?directives config =
  let config = update_head ?directives config in
  { (weak_flow config)
    with
      priority = 3;
      color = config.flow_color;
      style = config.flow_style ;
      width = (float_of_int config.flow_width);
      tags = TagMap.add "flow" (IntSet.add 1 IntSet.empty) TagMap.empty}

let strong_flow ?directives config =
  let config = update_head ?directives config in
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

let unify_id (g1,g2) =
  let nitems = max g1.nitems g2.nitems in
  {g1 with nitems = nitems},{g2 with nitems = nitems}

let is_empty g = IdMap.is_empty g.agents

let dummy_txt_item config =
  {
    dummy_item with
    width = float_of_int (config.txt_font)/.50.;
    height = float_of_int (config.txt_font)/.50.;
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
          | FontColor s -> {item with fontcolor = s}
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
  id,{
    remanent
    with
      sig_items  = IdMap.add id item remanent.sig_items ;
      nsig_items = id
  }

let p_txt x =
  match x with
  | Comment _ | Tag _ | Direction _  | Set_scale _ | Scale _ | Radius _ -> false
  | Shape _ | FontColor _ | FillColor _ | Color _ | Fontsize _  | Width _ | Height _ -> true

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

let add_agent_type name ?directives:(attributes=[]) remanent =
  let ag_c = remanent.nagent_sig_items +1 in
  let remanent = {remanent with nagent_sig_items = ag_c} in
  let f id item = {item with label = escaped name (Some item.fontsize) ; short_label = name } in
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

let add_son_type father error1 error2 error3 name dummy ?directives:(attributes=[]) p color_list remanent =
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
      {sibbling_item with father = Some father ; label = escaped name (Some sibbling_item.fontsize) ; short_label = name }
    in
    let s_id,remanent = add_sig (fun _ x -> x) sibbling_item remanent in
    let father_item =
      { father_item with sibblings =
                           add_sibbling s_id s_id father_item.sibblings}
    in
    s_id,{remanent with sig_items = IdMap.add father father_item remanent.sig_items}

let add_site_type agent_type name ?directives:(attributes=[]) remanent =
  add_son_type agent_type "add_site_type" "a site" "agent" name (dummy_site_name remanent.config) ~directives:attributes p_site_type remanent.config.site_colors remanent

let add_internal_state_type site_type state ?directives:(attributes=[]) remanent = add_son_type site_type "add_internal_state_type" "an internal state" "site" state (dummy_state_kind remanent.config) ~directives:attributes p_state_type remanent.config.state_colors remanent

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

let add_agent agent_type abs ord ?directives:(attributes=[]) remanent =
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

let add_empty_graph abs ord remanent =
  let item = {(dummy_txt_item remanent.config)
              with
               kind = Empty_agent ;
               coordinate = {abscisse = abs ; ordinate=ord} ;
               label = escaped remanent.config.empty_graph (Some remanent.config.dummy_font) ;
               fontsize = remanent.config.dummy_font } in
  add_node (fun _ x -> x) item remanent

let add_empty_node abs ord remanent =
  let item = {(dummy_txt_item remanent.config)
              with
               coordinate = {abscisse = abs ; ordinate=ord} ;
               label = "" ;
               fontsize = remanent.config.dummy_font } in
  add_node (fun _ x -> x) item remanent

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

let add_site agent name ?directives:(attributes=[]) remanent =
  add_son agent name (Site name) "add_site" "a site" "agent" attributes p_site remanent

let add_internal_state site_type state ?directives:(attributes=[])  remanent =
  add_son site_type state (State site_type) "add_internal_state" "an internal state" "site" attributes p_state remanent

let op edge =
  {edge
   with
    backward = edge.forward ;
    forward = edge.backward ;
    corner1 = edge.corner2 ;
    corner2 = edge.corner1 ;
    h_type = edge.t_type ;
    t_type = edge.h_type }

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

let merge_type a b =
  match a,b
  with
  | Normal, _ -> b
  | _,Normal -> a
  | Vee,Vee -> Vee

let fusion_edge s1 s2 =
  {s2 with forward = s1.forward || s2.forward ; backward = s1.backward || s2.backward ; h_type = merge_type s1.h_type s2.h_type ; t_type = merge_type s1.t_type s2.t_type}

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
let add_proj_elt ?name:(name=None) ?ca:(ca=None) ?cb:(cb=None) ?donotfuse:(donotfuse=false) config = add_link (projection ~name:name ~ca:ca ~cb:cb ~donotfuse:donotfuse config)
let add_edge x y z = add_relation (link z.config) x y z
let add_weak_flow_and_link ?directives x y z = add_relation (weak_flow ?directives z.config) x y (add_edge  x y z)
let add_flow_and_link ?directives x y z = add_relation (flow ?directives z.config) x y (add_weak_flow_and_link ?directives x y z)
let add_strong_flow_and_link ?directives x y z = add_relation (strong_flow ?directives z.config) x y (add_flow_and_link ?directives x y z)

let add_weak_flow ?directives x y z = add_relation (weak_flow ?directives z.config) x y z
let add_flow ?directives x y z = add_relation (flow z.config) x y (add_weak_flow ?directives x y z)
let add_strong_flow ?directives x y z = add_relation (strong_flow ?directives z.config) x y (add_flow ?directives x y z)

let color_of_edge e = e.color
let corner1_of_edge e =
  match e.corner1
  with None | Some "" -> ""
     | Some s -> (":"^s)

let corner2_of_edge e =
  match e.corner2
  with None | Some "" -> ""
     | Some s -> (":"^s)

let pen_width_of edge =
  if edge.width < 2. then "1"
  else string_of_int (int_of_float edge.width)

let add_free site_id ?directives:(attributes=[]) remanent =
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

let add_bound site_id ?directives:(attributes=[]) remanent =
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



let insert_text_here s x y ?directives:(d=[]) remanent =
  let item = parse_attributes p_txt "txt" d (dummy_txt_item remanent.config) in
  let item =
    {item
     with label = escaped s (Some item.fontsize) ;
          coordinate = {abscisse = x ; ordinate = y}}
  in
  let f _ x = x in
  let id,remanent = add_node f item remanent in
  remanent



let add_binding_type site_id site_type ?directives:(attributes=[]) remanent =
  let id,remanent = add_bound site_id ~directives:attributes remanent in
  match
    IdMap.find_option id remanent.items,
    IdMap.find_option site_type remanent.sig_items
  with
  | Some node,Some s_type ->
    begin
      let ag_string =
        match s_type.father with
        | None -> let _ = Printf.fprintf stderr "ERROR: in add_binding_type, wrong site type.\n" in
          "*"
        | Some id ->
          begin
            match IdMap.find_option id remanent.sig_items
            with
              None ->
              let _ = Printf.fprintf stderr "ERROR: in add_binding_type, dandling pointers\n" in
              "*"
            | Some node -> node.short_label

          end
      in
      let s_string = s_type.short_label in
      let d = to_degree node.orientation in
      let p = node.coordinate in
      let p' =
        if d <= 90. || d>=270. then
          {p with ordinate = p.ordinate +. 0.023*.(float_of_int remanent.config.binding_type_font)-.(dummy_txt_item remanent.config).height/.2.}
        else
          {p with ordinate = p.ordinate -. 0.023*.(float_of_int remanent.config.binding_type_font)+.(dummy_txt_item remanent.config).height/.2.}
      in
      let ag_string,s_string = escape_short ag_string s_string in
      let txt = ag_string^"."^s_string in
      let remanent = insert_text_here txt p'.abscisse p'.ordinate ~directives:[Fontsize remanent.config.binding_type_font] remanent in
      id,remanent
    end
  | _,_ ->
    let _ = Printf.fprintf stderr "ERROR: in add_binding_type, dandling pointers\n" in
    id,remanent

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
  let arrowhead =
    match dir_of_edge edge,edge.h_type with
    | ("none" | "both" | "forward"),_ | _,Normal -> ""
    | "back",Vee -> ",arrowhead=\"vee\""
    in
  let arrowtail =
    match dir_of_edge edge,edge.t_type with
    | ("none" | "both" | "back") ,_ | _,Normal -> ""
    | "forward",Vee -> ",arrowtail=\"vee\""
  in
  Printf.fprintf log
    "%s%s -> %s%s [dir = \"%s\",color=\"%s\",penwidth=%s,label=\"%s\",style=\"%s\"%s%s%s];\n"
    s1 (corner1_of_edge edge) s2 (corner2_of_edge edge) (dir_of_edge edge)
    (color_of_edge edge) (pen_width_of edge)
    (escape_color edge.comment (color_of_edge edge))
    (edge.style)
    (match edge.h_scale
     with
     | 1. ->  ""
     | f  -> ",arrowsize=\""^(string_of_float f)^"\"")
    arrowhead arrowtail


let dump_node log filter filter_label filter_color node remanent =
  Printf.fprintf log
    "fixedsize=true,\nlabel = \"%s\",\nfontsize=%i,\npos=\"%f,%f!\",\nwidth=%f,\nheight=%f,\nshape=\"%s\",\nstyle=\"%s\",\nfillcolor=%s,\ncolor=%s,\nfontcolor=%s" (filter_label node.label) node.fontsize node.coordinate.abscisse node.coordinate.ordinate (node.width*.node.scale_factor)  (node.height*.node.scale_factor)  node.shape node.style (filter_color node.fillcolor)  node.color node.fontcolor

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
    let rec aux l accu accu2  =
      match
        l
      with
        edge::q ->
        if edge.priority < threshold
        then
          aux q accu accu2
        else
          begin
            if edge.corner1 <> None || edge.corner2 <> None || not edge.canbefused

            then
              aux q accu (edge::accu2)
            else
              let accu  =
                match accu
                with None -> Some edge
                   | Some accu ->
                     Some (fusion_edge accu edge)
              in
              aux q accu accu2
          end
      | [] -> accu,accu2
    in
    let a,b = aux l None [] in
    let _ =
      match a
      with
        None -> ()
      | Some edge ->
        dump_edge
          chan
          ((n1:string),(n2:string))
          edge
          remanent
    in
    List.iter (fun edge -> dump_edge chan (n1,n2) edge remanent) b

let fill_empty g =
  if is_empty g
  then
    let a,b = add_empty_graph 0. 0. g in
    [a],b
  else
    [],g

let dump file ?flags:(filter=[]) remanent =
  let chan = open_out file in
  let _,remanent = fill_empty remanent in
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



let new_internal_state_type agent_type site_type state ?directives:(d=[]) (state_list,remanent) =
  let id,remanent = add_internal_state_type site_type state ~directives:d remanent in
  (id::state_list),remanent

let new_site_type agent_type site_type state_list ?directives:(d=[]) (site_list,remanent) =
  let id,remanent = add_site_type agent_type site_type ~directives:d remanent in
  let list,remanent =
    List.fold_left
      (fun (list,remanent) (state,d) ->
         new_internal_state_type agent_type id state ~directives:d (list,remanent)) ([],remanent) state_list
  in
  (id,List.rev list)::site_list,remanent

let new_agent_type agent_type site_list ?directives:(d=[]) (agent_list,remanent) =
  let id,remanent = add_agent_type agent_type ~directives:d remanent in
  let list,remanent =
    List.fold_left
      (fun (list,remanent) (site,d,state_list ) ->
         new_site_type id site state_list ~directives:d (list,remanent))
      ([],remanent) site_list
  in
  (id,List.rev list)::agent_list,remanent

let add_in_signature (signature:signature) remanent =
  let list,remanent =
    List.fold_left
      (fun (list,remanent) (agent,d,site_list) ->
         new_agent_type agent site_list ~directives:d (list,remanent))
      ([],remanent)
      signature
  in List.rev list,remanent

let new_state agent site (state_list,remanent) state  =
  let id,remanent =
    match state
    with
    | Free_site op  -> add_free site ~directives:op remanent
    | Bound_site op -> add_bound site ~directives:op remanent
    | Internal_state (op1,op2) -> add_internal_state site op1 ~directives:op2  remanent
  in
  id::state_list,remanent

let new_site agent (site_list,remanent) (site,directives,states)   =
  let site,remanent = add_site agent site ~directives:directives remanent in
  let states,remanent  =
    List.fold_left
      (new_state agent site)
      ([],remanent)
      states
  in
  (site,List.rev states)::site_list,remanent

let new_agent (agent_list,remanent) (ag,op1,op2,op3,l)  =
  let agent,remanent = add_agent ag op1 op2 ~directives:op3 remanent in
  let sites,remanent = List.fold_left (new_site agent) ([],remanent) l in
  (agent,List.rev sites)::agent_list,remanent

let add_in_graph l remanent =
  let l,remanent =
    List.fold_left new_agent ([],remanent) l
  in
  List.rev l,remanent


let edge_list f l remanent =
  List.fold_left (fun remanent (x,y) -> f x y remanent) remanent l

let add_flow_list ?directives =
  edge_list
    (add_flow ?directives)
let add_flow_and_link_list ?directives =
  edge_list
    (add_flow_and_link ?directives)
let add_strong_flow_list ?directives =
  edge_list
    (add_strong_flow ?directives)
let add_weak_flow_list ?directives =
  edge_list
    (add_weak_flow ?directives)
let add_strong_flow_and_link_list ?directives =
  edge_list
    (add_strong_flow ?directives)
let add_link_list  =
  edge_list add_edge
let add_weak_flow_list ?directives =
  edge_list
    (add_weak_flow ?directives)
let add_weak_flow_and_link_list ?directives =
  edge_list
    (add_weak_flow_and_link ?directives)

let add_free_list l remanent =
  List.fold_left
    (fun (list,remanent) (site,directives) ->
       let a,remanent = add_free site ~directives:directives remanent in
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

let color s ?flags:(filter=[]) remanent =
  map_node
    (fun item ->
       let tags = item.tags in
       if filter_tags filter tags
       then
         {item with color = s}
       else
         item)
    (fun edge ->
       let tags = edge.tags in
       if filter_tags filter tags
       then
         {edge with color = s}
       else
         edge)
    remanent

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
let corners_p p remanent =
  let fusion (x,y,z,t) (x',y',z',t') = (min x x',max y y',min z z',max t t')
  in
  fold_node
    (fun node pos ->
       if p node then
         let x,y=node.coordinate.abscisse,node.coordinate.ordinate in
         let coords =
           x-.node.width/.2.,x+.node.width/.2.,y-.node.height/.2.,y+.node.height/.2.
         in
         match pos
         with None -> Some coords
            | Some old -> Some (fusion coords old)
       else pos)
    remanent
    None

let corners = corners_p (fun _ -> true)

let size remanent =
  match corners remanent
  with None -> 0.,0.
     | Some (x,y,z,t) -> y-.x,t-.z

let pseudo_equal a b =
  abs ((a-.b)/.(abs (a) +. (abs b))) < 0.01

let rotate point angle remanent =
  let degree = to_degree angle in
  let () =
    if not
        ((pseudo_equal degree 0.)
         || (pseudo_equal degree 90.)
         || (pseudo_equal degree 180.)
         || (pseudo_equal degree 270.)
         || (pseudo_equal degree 360.))
    then
      Printf.fprintf stderr "Warning: Rotation is only implemented for right, straight, and full angles\n"
  in
  let cos = cos (to_radius angle) in
  let sin = sin (to_radius angle) in
  let degree = to_degree angle in
  let
    swap
    =
    if (degree > 45. && degree < 135.)
    || (degree > 225. && degree < 270.)
    then
      (fun x y -> y)
    else
      (fun x y -> x)
  in
  map_node
    (fun node ->
       let deltax = node.coordinate.abscisse -. point.abscisse in
       let deltay = node.coordinate.ordinate -. point.ordinate in
       {node with
        coordinate =
          {node.coordinate
           with
            abscisse = point.abscisse +. sin*.deltay +. cos*.deltax ;
            ordinate = point.ordinate +. cos*.deltay +. sin*.deltax ;
          };
        width = swap node.width node.height ;
        height = swap node.height node.width ;
        orientation = of_degree ((to_degree angle) +. (to_degree node.orientation))})
    (fun node ->
       let deltax = node.coordinate.abscisse -. point.abscisse in
       let deltay = node.coordinate.ordinate -. point.ordinate in
       {node with
        coordinate =
          {node.coordinate
           with
            abscisse = point.abscisse +. sin*.deltay +. cos*.deltax ;
            ordinate = point.ordinate +. cos*.deltay +. sin*.deltax ;};
        width = swap node.width node.height ;
        height = swap node.height node.width;
        orientation = of_degree ((to_degree angle) +. (to_degree node.orientation))})
    remanent
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

let rec translate_node vector id remanent =
  let node = IdMap.find_option id remanent.items in
  match node
  with
  | None -> let _ = Printf.fprintf stderr "Warning: In translate_node, unknown node.\n" in remanent
  | Some node ->
    IdMap.fold
      (fun _ -> IdSet.fold
          (translate_node vector))
      node.sibblings
      {
        remanent
        with
          items = IdMap.add id {node with coordinate = translate node.coordinate vector} remanent.items}

let translate_agent vector ag =
  translate_node vector ag

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

let disjoint_union_list l =
  match l
  with
    [] -> None
  | t::q ->
    let inj_list,g =
      List.fold_left
        (fun (inj_list,g) g' ->
           let inj,inj',g'' = disjoint_union g g' in
           inj'::List.rev_map
             (compose_lift inj)
             (List.rev inj_list),
           g'')
        ([(fun x->x),(fun y->y),(fun z->z)],t)
        q
    in Some (List.rev inj_list,g)

let add_match ?color:(color="") ?style:(style="") l remanent =
  let config =
    if color = ""
    then remanent.config
    else {remanent.config with pairing_color = color}
  in
  let config =
    if style = ""
    then config
    else {config with pairing_style = style }
  in
  List.fold_left
    (fun remanent (x,y) -> add_match_elt remanent.config x y remanent) remanent l


let add_proj ?color:(color="") ?style:(style="") ?name:(name=None)   ?ca:(ca=None) ?cb:(cb=None) ?donotfuse:(donotfuse=false) l remanent =
  let config =
    if color = ""
    then remanent.config
    else {remanent.config with projection_color = color}
  in
  let config =
    if style = ""
    then config
    else {config with projection_style = style }
  in
  fst (List.fold_left
         (fun (remanent,name) (x,y) ->
            let q,name =
              match name
              with None -> None,None
                 | Some [] -> None,None
                 | Some (t::q) -> Some q,t
            in
            (add_proj_elt ~name:name ~ca:ca ~cb:cb ~donotfuse:donotfuse config x y remanent,q)) (remanent,name) l)

let add_emb ?color:(color="") ?style:(style="") ?ca:(ca=None) ?cb:(cb=None) ?donotfuse:(donotfuse=false) l remanent =
  let config =
    if color = ""
    then remanent.config
    else {remanent.config with projection_color = color}
  in
  let config =
    if style = ""
    then config
    else {config with projection_style = style}
  in
  List.fold_left
    (fun remanent (x,y) -> add_proj_elt ~ca:ca ~cb:cb config x y remanent) remanent l

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
    ~directives:[Width (0.);Height(0.);FillColor("white");Color("white")]
    remanent

let dummy_node_type = 1

let filter_out_direction =
  List.filter
    (fun x -> match x with Direction _ -> false | _ -> true)

let add_dummy_agent string x y directives remanent =
  let f id item = item in
  let item = parse_attributes (fun _ -> true) "" directives { dummy_item with coordinate = {abscisse = x ; ordinate = y}} in
  add_node f {item with width = 0. ; height = 0. ; color = "white" ; style = ""} remanent

let p_agent ag =
  match ag.kind
  with
  | Agent _ | Empty_agent -> true
  | _ -> false

let add_rule x y ?reversible:(reversible=false) ?directives:(directives=[]) remanent =
  let item = {dummy_item with coordinate = {abscisse = x ; ordinate = y} ; width = remanent.config.rule_length ; height = remanent.config.rule_length ; orientation = e ; fontsize = remanent.config.rule_name_font} in
  let item = parse_attributes (fun _ -> true) "" directives item  in
  let size = item.width*.item.scale_factor in
  let angle' = item.orientation in
  let angle = angle'.radius in
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
    let g =
      add_link
        {(rule remanent.config)
         with width = item.width ;
              color = item.color ; tags = item.tags } node_id2 node_id1 remanent
    in
    let g =
      if reversible
      then
        add_link
          {(rule remanent.config) with
           width = item.width ;
           color = item.color ; tags = item.tags }
          node_id1 node_id2 g
      else
        g
    in
    if item.comment = ""
    then
      g
    else
      let angle = anticlockwise angle' 90. in
      let cangle = cos (to_radius angle) in
      let cangle =
        if cangle > 0. then cangle else -.cangle
      in
      let d =
        (0.023*.(float_of_int remanent.config.binding_type_font)+.(dummy_txt_item remanent.config).height/.2.)*.cangle
      in
      let c = point_on_ellipse {abscisse=x;ordinate=y} d d angle 1. in
      insert_text_here item.comment c.abscisse c.ordinate g

let put_a_cross x y x' y' remanent =
  let directives = [] in
  let width = float_of_int remanent.config.cross_width in
  let comment = "" in
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

let cross remanent =
  let directives = [] in
  let comment = "" in
  let width = float_of_int remanent.config.cross_width in
  match corners remanent
  with
  | None -> remanent
  | Some (x,x',y,y') ->
    put_a_cross x y x' y' remanent

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

let move graph cx cy =
  match corners_p p_agent graph
  with
    None -> graph
  | Some (minx,maxx,miny,maxy) ->
    let x = (maxx+.minx)/.2. in
    let y = (maxy+.miny)/.2. in
    translate_graph {abscisse=cx-.x;ordinate=cy-.y} graph

let bind (extrag1,g1) (extrag2,g2) c1 c2 i1 i2 style color g =
  add_emb ~color:color ~style:style ~ca:(Some c1) ~cb:(Some c2)
    (match extrag1,extrag2
     with [a],[b] -> [i1 a,i2 b]
        | [a],_ ->let a=i1 a in
          IdMap.fold
            (fun _ -> IdSet.fold (fun b l-> (a,i2 b)::l))
            g2.agents
            []
        | _,[b] -> let b=i2 b in
          IdMap.fold
            (fun _ -> IdSet.fold (fun a l -> (i1 a,b)::l))
            g1.agents
            []
        | _,_ ->  IdMap.fold
                    (fun _ -> IdSet.fold (fun a l -> (i1 a,i2 a)::l))
                    g1.agents
                    []
    ) g


let neutral_extension x = ([],[],[]),x
let build_rule ?file:(file="") ?hgap:(hgap=None)  ?vgap:(vgap=None) ?explicit:(explicit=false) ?(rule_symb=true) ?reversible:(reversible=false) ?directives:(directives=[]) domain ?extend_domain:(extend_domain=neutral_extension)  extend_lhs extend_rhs  =
  let (stylel,colorl),(styler,colorr) = domain.config.rule in
  let c = domain.config.rule_corners in
  let node = {dummy_item with orientation = e ; width =  domain.config.rule_length} in
  let node = parse_attributes (fun _ -> true) "" directives node in
  let (rule_width:float),(rule_margin:float) =
    let v =
      match hgap
      with
      | None -> 1.
      | Some v -> v
    in
    let a,b = node.width(**.scale*),domain.config.rule_margin in
    a*.v/.(a+.b),
    b*.v/.(a+.b)
  in
  let node = {node with width = rule_width} in
  let alpha =
    match vgap
    with None -> 0.75
       | Some (a:float) ->
         a/.(rule_width+.2.*.rule_margin)
  in
  let angle = sample_angle node.orientation in
  let (l1,l2,l3),lhs = extend_lhs domain in
  let (r1,r2,r3),rhs = extend_rhs domain in
  let (d1,d2,d3),domain = extend_domain domain in
  let extra_domain,domain = fill_empty domain in
  let extra_lhs,lhs = fill_empty lhs in
  let extra_rhs,rhs = fill_empty rhs in
  let correct extra l  =
    match extra
    with
    | [a] -> [a,[]]
    | [] -> l
    | _ -> failwith "bug in build_rule"
  in
  let l1 = correct extra_lhs l1 in
  let r1 = correct extra_rhs r1 in
  let d1 = correct extra_domain d1 in
  let lhs = tag_all_nodes "lhs" 1 lhs in
  let rhs = tag_all_nodes "rhs" 1 rhs in
  let cornerlhs =
    match
      corners lhs,corners_p p_agent rhs
    with
      Some (xm,xM,_,_),Some (_,_,ym,yM) -> xm,xM,ym,yM
    | None,_ | _,None -> 0.,0.,0.,0.
  in
  let cornerrhs =
    match
      corners rhs,corners_p p_agent rhs
    with
      Some (xm,xM,_,_), Some(_,_,ym,yM) -> xm,xM,ym,yM
    | None,_ | _,None -> 0.,0.,0.,0.
  in
  let distance = (rule_width +.rule_margin*.2.) *. node.scale_factor  in
  let rulex,ruley,deltax,deltay,distancex,distancey = compute_padding cornerlhs cornerrhs angle distance in
  let sigmal,sigmar,rule = disjoint_union lhs (translate_graph {abscisse=deltax;ordinate=deltay} rhs) in
  let rule = if rule_symb then add_rule rulex ruley ~reversible:reversible ~directives:directives rule else rule in
  let sigma,rule =
    if explicit
    then
      let domain = move domain (rulex-.distancey*.alpha)
          (ruley-.distancex*.alpha) in
      let _,inj,rule = disjoint_union domain rule in
      inj,rule
    else
      ((fun x->x),(fun x->x),(fun x->x)),rule
  in
  let sigmal = compose_lift sigma sigmal in
  let sigmar = compose_lift sigma sigmar in
  let rule =
    if explicit
    then
      let rule = bind (extra_domain,domain) (extra_lhs,lhs) (rotate_co c.d_l angle) (rotate_co c.l_d angle) (fun x->x) (lift_agent sigmal) stylel colorl rule in
      let rule = bind (extra_domain,domain) (extra_rhs,rhs) (rotate_co c.d_r angle) (rotate_co c.r_d angle) (fun x->x) (lift_agent sigmar) styler colorr rule in
      rule
    else rule
  in
  let _ =
    if file=""
    then
      ()
    else
      dump file rule
  in
  sigmal,
  sigmar,
  (List.concat [d1;lift_agent_list sigmal l1;lift_agent_list sigmar r1],
   List.concat [d2;lift_site_list sigmal l2;lift_site_list sigmar r2],
   List.concat [d3;lift_state_list sigmal l3;lift_state_list sigmar r3]),
  rule




let build_losange ?file:(file="") ?hgap:(hgap=None) ?vgap:(vgap=None) ?bottom:(bottom=None) extend_left extend_right ?extend_top:(extend_top=(fun _ _ g -> ([],[],[]),g)) ?top:(top=None) ?piv_left:(piv_left=(fun x->x)) ?piv_right:(piv_right=(fun x->x)) graph =
  let c = graph.config.losange_corners in
  let hpadding =
    match hgap with
      None -> graph.config.losange_padding
    | Some a -> a
  in
  let vpadding =
    match vgap with
      None -> graph.config.losange_padding
    | Some a -> a
  in
  let (style_1,color_1),(style_2,(color_2l,color_2r)),(style_3,(color_3l,color_3r)),(style_4,(color_4l,color_4r)),(style_5,(color_5l,color_5r)),(style_6,color_6) = graph.config.losange in

  let g_bottom' =
    match bottom
    with
    | None -> None
    | Some a -> Some graph
  in
  let node_bottom,g_bottom =
    match bottom
    with
    | None -> None,graph
    | Some a ->
      let a,b = a graph in Some a,b
  in
  let (node_left:valuation),g_left = extend_left g_bottom in
  let g_left,g_bottom = unify_id (g_left,g_bottom) in
  let (node_right:valuation),g_right = extend_right g_bottom in
  let node_top,g_top = extend_top node_left node_right (fuse (piv_left g_left) (piv_right g_right)) in
  let g_top' =
    match top
    with
    | None -> None
    | Some a -> Some(a node_left node_right node_top g_left)
  in
  let ltop,(proj_top,_,_) =
    match g_top'
    with
    | None -> [],((fun x -> x),(fun x -> x),(fun x -> x))
    | Some ((x:valuation),a,f) -> [a],f node_left node_right
  in
  let extra_bottom,g_bottom = fill_empty g_bottom in
  let extra_left,g_left = fill_empty g_left in
  let extra_right,g_right = fill_empty g_right in
  let extra_top,g_top = fill_empty g_top in
  let sizex,sizey =
    match corners_p p_agent g_top
    with
      None -> 0.,0.
    | Some (minx,maxx,miny,maxy) -> maxx-.minx,maxy-.miny
  in
  let alpha = match g_bottom',g_top' with None,None -> 2. | _ -> 1. in
  let g_bottom' =
    match g_bottom'
    with
    | None -> None
    | Some g -> Some (move g 0. (-3.*.vpadding))
  in
  let g_bottom = move g_bottom 0. (alpha*.(-1.)*.vpadding) in
  let g_right = move g_right (3.*.hpadding) 0. in
  let g_left = move g_left (-3.*.hpadding) 0. in
  let g_top = move g_top 0. (alpha*.vpadding) in
  let g_top' =
    match g_top' with None -> None | Some (a,b,_) -> Some (a,move b 0. (3.*.vpadding))
  in
  let g_bottom' =
    match g_bottom' with None -> None | Some (a) -> Some (move a  0. (-3.*.vpadding))
  in
  let list=
    (match g_bottom'
     with None -> (fun x -> x)
        | Some g -> (fun x -> g::x))
      (g_bottom::g_left::g_right::g_top::(match g_top' with None -> [] | Some (_,g) -> [g]))
  in
  let g = disjoint_union_list list in
  let g,l =
    match g with None -> raise Exit
               | Some (l,g) ->
                 let l = List.rev_map lift_agent (List.rev l) in
                 begin
                   match g_bottom',l with
                   | None,_ | _,[]-> g,l
                   | Some g_bottom',t::t1::tl::tr::q ->
                     let extra_bottom',g_bottom' = fill_empty g_bottom' in
                     let g = bind (extra_bottom',g_bottom') (extra_bottom,g_bottom) c.b'_b c.b_b' t t1  style_1 color_1 g in
                     let g = bind (extra_bottom',g_bottom') (extra_left,g_left) c.b'_l c.l_b'  t tl style_2 color_2l g in
                     let g = bind (extra_bottom',g_bottom') (extra_right,g_right) c.b'_r c.r_b' t tr style_2 color_2r g in
                     g,t1::tl::tr::q
                   | _ -> failwith "internal error in build_losange"
                 end
  in
  match l with
    l_bottom::l_left::l_right::l_top::q ->
    let l_top_proj = compose l_top proj_top in
    let g = bind (extra_bottom,g_bottom) (extra_left,g_left) c.b_l c.l_b l_bottom l_left style_3 color_3l g in
    let g = bind (extra_bottom,g_bottom) (extra_right,g_right) c.b_r c.r_b l_bottom l_right style_3 color_3r g in
    let g = bind (extra_left,g_left) (extra_top,g_top) c.l_t c.t_l l_left l_top style_4 color_4l g in
    let g = bind (extra_right,g_right) (extra_top,g_top) c.r_t c.t_r l_right l_top style_4 color_4r g in
    let g =
      match g_top',q with
        None,_ | _,[] -> g
      | Some (_,g_top'),l_top'::_ ->
        let extra_top',g_top' = fill_empty g_top' in
        let g = bind (extra_left,g_left) (extra_top',g_top') c.l_t' c.t'_l l_left l_top' style_5 color_5l g in
        let g = bind (extra_right,g_right) (extra_top',g_top') c.r_t' c.t'_r l_right (compose l_top' proj_top) style_5 color_5r g in
        let g = bind (extra_top,g_top) (extra_top',g_top') c.t_t' c.t'_t l_top (compose l_top' proj_top) style_6 color_6 g in
        g
    in
    let (id:lift) = (fun x-> x),(fun y->y),(fun z->z) in
    let _ = if file <> "" then dump file g in
    (Some id,id,id,id,id,Some id),
    (([],[],[]):valuation),g
  | _ -> failwith "internal error in build_losange"

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

let int_of_agent x = x
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
  let rulex,ruley,deltax,deltay,_,_ = compute_padding (xm,xM,ym,yM) (xm',xM',ym',yM') angle distance in
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
      let _ = dump (s^"0.dot") ~flags:["rule_flow",0;"sp_flow",0] rule_species in
      let _ = dump (s^"1.dot") ~flags:["rule_flow",1;"sp_flow",0] rule_species in
      let _ = dump (s^"2.dot") ~flags:["rule_flow",1;"sp_flow",0] rule_species_with_proj in
      let _ = dump (s^"3.dot") ~flags:["rule_flow",1;"sp_flow",1] rule_species_with_proj in
      let _ = dump (s^"4.dot") ~flags:["rule_flow",1;"sp_flow",1] rule_species in
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

let insert_text_on_boarder list ?padding:(padding=1.) ?inside:(x=Outside) ?directives:(d=[])  graph =
  match corners graph
  with
  | None ->
    List.fold_left
      (fun graph (s,_) -> insert_text_here s 0. 0. ~directives:d graph)
      graph list
  | Some (x_min,x_max,y_min,y_max) ->
    let x = (x_min+.x_max)/.2. in
    let y = (y_min+.y_max)/.2. in
    let dx = (x_max -. x_min)*. padding in
    let dy = (y_max -. y_min)*. padding in
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
         insert_text_here s center.abscisse center.ordinate ~directives:d remanent )
      graph list


let set_co init mode =
  {init with config =
               match mode
               with Empty -> {init.config with losange_corners = empty_co}
                  | Middle -> {init.config with losange_corners = middle_co}
                  | Corners -> {init.config with losange_corners = corners_co}}

let set_ru init mode =
  {init with config =
               match mode
               with Empty -> {init.config with rule_corners = empty_ru}
                  | Middle -> {init.config with rule_corners = middle_ru}
                  | Corners -> {init.config with rule_corners = corners_ru}}
