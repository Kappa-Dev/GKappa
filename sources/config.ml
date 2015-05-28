(**
 * config.ml 
 * GKappa
 * Jérôme Feret, projet Antique, INRIA Paris-Rocquencourt
 * 
 * Creation:                      <2015-03-29 feret>
 * Last modification: Time-stamp: <2015-05-27 23:13:12 feret>
 * * 
 *  
 * Copyright 2015 Institut National de Recherche en Informatique  * et en Automatique.  All rights reserved.  
 * This file is distributed under the terms of the 
 * GNU Library General Public License *)


(** GKappa is a OCAML library to help making slides in Kappa *)
(** More applications are coming soon (hopefully) *)

(** Config file for GraphViz output *)
 
include Gkappa

let config = 
  {
    agent_colors = ["magenta";"blue";"green";"purple";"darkgreen";]; 
    site_colors = ["white";"cyan";"yellow";"pink"];
    state_colors = ["white";"black"];
    show_agent_names = true ; 
    show_site_names = false ;
    show_state_names = false ;
    show_free_symbols = true ;
    color_agents = true ;
    color_sites = true ;
    color_states = true ;
    site_width = 0.4 ;
    site_height = 0.4;
    agent_width = 2. ;
    agent_height = 1. ;
    state_width = 0.1 ;
    state_height = 0.1 ; 
    pi = 3.1416 ;
    free_width = 0.15 ; 
    free_height = 0.1 ;
    bound_height = 0.3 ;
    rule_length = 1.5 ;
    rule_width = 1;
    cross_width = 5 ; 
    edge_label_font = 50 ;
    link_width = 2 ;
    pairing_style = "dashed";
    pairing_color = "cyan";
    pairing_width = 2;
    weak_flow_color = "cyan";
    weak_flow_style = "dashed";
    flow_color = "red";
    strong_flow_color = "red";
    strong_flow_style = "";
    flow_style = "";
    agent_label_font = 20 ; 
    site_label_font = 14;
    state_label_font = 10 ;
    dummy_font = 20;
    rule_margin = 0.1; 
    flow_padding = 0.05;
    projection_width = 2;
    rule_color = "black";
    rule_style = "" ;
    projection_color = "cyan";
    projection_style = "dashed";
    weak_flow_width = 1;
    flow_width = 2;
    strong_flow_width =3;
  }
