(**
 * gkapa.mli
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
| Fontsize of int 
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
type state_type = 
| Free of directive list 
| Bound of directive list
| Internal of internal_state_type * directive list
    
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
val init: config -> agent_type * remanent_state 
val add_agent_type: string -> directive list -> remanent_state -> agent_type * remanent_state 
val add_site_type: agent_type -> string -> directive list -> remanent_state -> site_type * remanent_state 
val add_internal_state_type: site_type -> string -> directive list -> remanent_state -> internal_state_type * remanent_state 
  
val add_in_signature: remanent_state -> signature -> remanent_state * signature_vars 
val add_in_graph: remanent_state -> graph -> remanent_state * graph_vars 
val add_agent: agent_type -> float -> float -> directive list -> remanent_state -> agent option * remanent_state 
val add_site: agent -> site_type  -> directive list -> remanent_state -> site option * remanent_state 
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
