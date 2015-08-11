(**
 * gkapa.mli
 * GKappa
 * Jérôme Feret, projet Antique, INRIA Paris-Rocquencourt
 * 
 * Creation: March, the 28th of 2015
 * Last modification: Time-stamp: <2015-07-15 09:15:43 feret>
 * * 
 *  
 * Copyright 2015 Institut National de Recherche en Informatique  * et en Automatique.  All rights reserved.  
 * This file is distributed under the terms of the 
 * GNU Library General Public License *)


(* GKappa is a OCAML library to help making slides in Kappa *)
(* More applications are coming soon (hopefully) *)

open Data_structures 
open Geometry 
open StdLabels 

type in_out = Inside | Outside 
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
type ru_corner = 
  { 
    d_l:string ; 
    d_r:string ; 
    l_d: string ; 
    r_d: string
  }
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
    losange: (string*string)*(string*(string*string))*(string*(string*string))*(string*(string*string))*(string*(string*string))*(string*string);
    losange_corners: lo_corner;
    losange_padding: float ;
    rule: (string*string)*(string*string);
    rule_corners: ru_corner
  }



type co_mode = Empty | Middle | Corners
				   

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
    
type id 
type agent_type
type site_type
type internal_state_type
type state_type = 
| Free_site of directive list 
| Bound_site of directive list
| Internal_state of internal_state_type * directive list
    
type agent 
type site
type state 
type remanent_state 
type signature_vars = (agent_type * (site_type * internal_state_type list) list) list 
type graph_vars = (agent * (site * state list ) list) list 
type signature = 
      (string * directive list * 
	 (string * directive list * 
	    ((string * directive list) list)) list) list 
type graph = 
  (agent_type *float*float*directive list* 
     (site_type * directive list* 
	state_type list) list) list
type lift 

val init: config -> agent_type * remanent_state

val add_in_signature: signature -> remanent_state -> signature_vars * remanent_state 

val add_agent_type: string -> ?directives:directive list -> remanent_state -> agent_type * remanent_state 
val add_site_type: agent_type -> string -> ?directives:directive list -> remanent_state -> site_type * remanent_state 
val add_internal_state_type: site_type -> string -> ?directives:directive list -> remanent_state -> internal_state_type * remanent_state 
  
val add_in_graph: graph -> remanent_state -> graph_vars * remanent_state 

val add_agent: agent_type -> float -> float -> ?directives:directive list -> remanent_state -> agent * remanent_state
val add_site: agent -> site_type  -> ?directives:directive list -> remanent_state -> site * remanent_state 
val add_internal_state: site -> internal_state_type -> ?directives:directive list -> remanent_state -> state * remanent_state 
val add_free: site -> ?directives:directive list -> remanent_state -> state * remanent_state 
val add_bound: site -> ?directives:directive list -> remanent_state -> state * remanent_state 
val add_binding_type: site -> site_type -> ?directives:directive list -> remanent_state -> state * remanent_state   

val add_free_list: (site * directive list) list -> remanent_state -> state list * remanent_state
val add_link_list: (site*site) list -> remanent_state -> remanent_state 

val set_co: remanent_state  -> co_mode -> remanent_state
val set_ru: remanent_state -> co_mode -> remanent_state
val empty_co: lo_corner 
val empty_ru: ru_corner

val is_empty: remanent_state -> bool

val unify_id: remanent_state * remanent_state -> remanent_state * remanent_state 

val add_strong_flow_and_link: site -> site -> remanent_state -> remanent_state 
val add_flow_and_link_list : (site*site) list -> remanent_state -> remanent_state
val add_flow_list: (site*site) list -> remanent_state -> remanent_state 

val map_id: (id -> id) -> remanent_state -> 
  (agent->agent)*(site->site)*(state->state)*remanent_state 


val lift_of_agent_map: (agent -> agent) -> lift				       
val int_of_agent: agent -> int 
val compose_lift: lift -> lift -> lift 
val lift_agent: lift -> agent -> agent 
val lift_site: lift -> site -> site 
val lift_state: lift -> state -> state     
val dump: string -> ?flags:(tag*int) list -> remanent_state -> unit
val rotate: float ->float-> float->remanent_state ->remanent_state
val translate_graph: point -> remanent_state -> remanent_state
val sym: float -> float ->remanent_state -> remanent_state
val fuse: remanent_state -> remanent_state -> remanent_state
val horizontal_swap: remanent_state -> remanent_state 
val vertical_swap: remanent_state -> remanent_state 
val disjoint_union: remanent_state -> remanent_state -> lift * lift * remanent_state 
val disjoint_union_with_match: remanent_state -> remanent_state -> lift * lift * remanent_state 

val add_match: ?color:string -> ?style:string -> (agent*agent) list -> remanent_state -> remanent_state 
val add_proj: ?color:string -> ?style:string -> ?name:(string option list option)-> ?ca:(string option) -> ?cb:(string option) -> ?donotfuse:(bool) ->  (agent*agent) list -> remanent_state -> remanent_state 
val add_emb: ?color:string -> ?style:string -> ?ca:(string option) -> ?cb:(string option) -> ?donotfuse:(bool) -> (agent*agent) list -> remanent_state -> remanent_state

val tag_all_nodes: tag -> int -> remanent_state -> remanent_state 
val move_remanent_right_to: float -> remanent_state -> remanent_state -> remanent_state
val move_remanent_left_to: float -> remanent_state -> remanent_state -> remanent_state 
val move_remanent_above: float -> remanent_state -> remanent_state -> remanent_state 
val move_remanent_bellow: float -> remanent_state -> remanent_state -> remanent_state 

val add_rule: float -> float -> ?reversible:bool -> ?directives:directive list -> remanent_state -> remanent_state
val corners: remanent_state -> (float * float * float * float) option 
val cross: remanent_state -> remanent_state 
val put_a_cross: float -> float -> float -> float -> remanent_state -> remanent_state 			       

type valuation = graph_vars * (site * state list) list * state list 
val build_rule: ?file:string -> ?hgap:(float option) -> ?vgap:(float option) -> ?explicit:bool -> ?rule_symb:bool ->  ?reversible:bool -> ?directives:directive list -> remanent_state -> ?extend_domain:(remanent_state -> valuation * remanent_state) -> (remanent_state -> valuation * remanent_state) -> (remanent_state -> valuation * remanent_state) ->  lift * lift * valuation * remanent_state

val build_losange: ?file:string  -> ?hgap:(float option) -> ?vgap:(float option) -> ?bottom:((remanent_state -> valuation * remanent_state) option) -> (remanent_state -> valuation * remanent_state) -> (remanent_state -> valuation * remanent_state) -> ?extend_top:(valuation -> valuation -> remanent_state -> valuation * remanent_state) -> ?top:((valuation -> valuation -> valuation -> remanent_state -> valuation * remanent_state * (valuation -> valuation -> lift)) option) -> ?piv_left:(remanent_state -> remanent_state) -> ?piv_right:(remanent_state -> remanent_state) -> remanent_state -> (lift option * lift * lift * lift * lift * lift option) * valuation * remanent_state 
val proj_flow_on_a_species: ?file:string -> ?padding:float -> ?angle:angle -> ?flow:(site * site) list -> remanent_state -> remanent_state -> (agent * agent) list -> lift * lift * remanent_state * remanent_state 

val proj_flow_on_a_contact_map: ?file:string -> ?padding:float -> ?angle:angle -> ?flow:(site * site) list -> remanent_state -> remanent_state -> lift * lift * remanent_state * remanent_state 

val insert_text_here: string -> float -> float -> ?directives:directive list -> remanent_state -> remanent_state
val insert_text_on_boarder: (string * angle) list  -> ?padding:float -> ?inside:in_out -> ?directives:directive list -> remanent_state -> remanent_state 
val color:string -> ?flags:((tag*int) list) -> remanent_state -> remanent_state 

val add_empty_graph: float -> float -> remanent_state -> agent * remanent_state 
val add_empty_node: float -> float -> remanent_state -> agent * remanent_state 
