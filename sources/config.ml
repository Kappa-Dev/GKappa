(**
 * config.ml 
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


(** GKappa is a OCAML library to help making slides in Kappa *)
(** More applications are coming soon (hopefully) *)

(** Config file for GraphViz output *)
 
include Gkappa.GKappa 

let config = 
  {agent_colors = ["magenta";"blue";"green";"purple";"darkgreen";]; 
       site_colors = ["white";"cyan";"yellow";"pink"];
       state_colors = ["white";"black"];
       show_agent_names = true ; 
       show_site_names = false ;
       show_state_names = false ;
       show_free_symbols = true ;
       color_agents = true ;
       color_sites = true ;
       color_states = true ;
       site_radius = 0.4 ;
       pi = 3.1416 ;
       free_width = 0.15 ; 
       free_height = 0.1 ;
       bound_height = 0.3 ;
       rule_length = 1.5 ;
       rule_width = 1;
       edge_label_font = 20 ;
  }
