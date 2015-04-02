(**
 * signature_egfr.ml 
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

(** signature of the early egfr model *)
open Config 


(* chemical species*)
let _,remanent = init Config.config 
(*signature*)
let egf,remanent = add_agent_type "EGF" [Width 1.2;Height 0.8;Shape "ellipse"] remanent 
let egf_r,remanent = add_site_type egf "r" [Direction s] remanent 
let egfr,remanent = add_agent_type "EGFR" [Width 1.5;Height 0.8;Shape "hexagon"] remanent
let egfr_l,remanent = add_site_type egfr "l" [] remanent 
let egfr_r,remanent = add_site_type egfr "r" [] remanent 
let egfr_Y48,remanent = add_site_type egfr "Y48" [] remanent 
let egfr_Y48_u,remanent = add_internal_state_type egfr_Y48 "u" [] remanent
let egfr_Y48_p,remanent = add_internal_state_type egfr_Y48 "p" [] remanent
let egfr_Y68,remanent = add_site_type egfr "Y68" [] remanent 
let egfr_Y68_u,remanent = add_internal_state_type egfr_Y68 "u" [] remanent
let egfr_Y68_p,remanent = add_internal_state_type egfr_Y68 "p" [] remanent
let shc,remanent = add_agent_type "ShC" [Width 1.;Height 1.;Shape "ellipse"] remanent 
let shc_pi,remanent = add_site_type shc "pi" [] remanent 
let shc_Y7,remanent  = add_site_type shc "Y7" [] remanent 
let shc_Y7_u,remanent = add_internal_state_type shc_Y7 "u" [] remanent
let shc_Y7_p,remanent = add_internal_state_type shc_Y7 "p" [] remanent
let grb2,remanent = add_agent_type "Grb2" [Width 1.5;Height 0.8;Shape "ellipse"] remanent 
let grb2_a,remanent = add_site_type grb2 "a" [] remanent 
let grb2_b,remanent = add_site_type grb2 "b" [] remanent 
let sos,remanent = add_agent_type "Sos" [Width 1.2;Height 0.8;Shape "ellipse"] remanent 
let sos_d,remanent = add_site_type sos "d" [] remanent 

