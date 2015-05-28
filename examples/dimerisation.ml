(**
 * dimerisation.ml 
 * GKappa
 * Jérôme Feret, projet Antique, INRIA Paris-Rocquencourt
 * 
 * Creation:                      <2015-03-28 feret>
 * Last modification: Time-stamp: <2015-05-28 06:35:45 feret> 
 * * 
 *  
 * Copyright 2015 Institut National de Recherche en Informatique  * et en Automatique.  All rights reserved.  
 * This file is distributed under the terms of the 
 * GNU Library General Public License *)


(** GKappa is a OCAML library to help making slides in Kappa *)
(** More applications are coming soon (hopefully) *)

(** small example to drow a dimerisation rule *)

open Config
open Geometry
open Signature_egfr  

let half_domain, 
  [
    egf1,[egf1_r,_];
    egfr1,[egfr1_l,_;egfr1_r,_]; 
  ]
  =
  add_in_graph 
    signature_egfr 
    [
      egf,0.,0.,[],
      [egf_r,[Direction e],[]];
      egfr,2.5,0.,[],
      [egfr_l,[Direction w;Scale 0.9],[];
       egfr_r,[Direction s],[]]]

let half_domain = add_link_list [egfr1_l,egf1_r] half_domain
let other_half_domain = move_remanent_bellow 0.5 (horizontal_swap half_domain) half_domain  
 
let sigma1,sigma2,lhs_domain = disjoint_union half_domain other_half_domain 

let _,_,_,remanent = 
  build_rule lhs_domain 
    (fun remanent -> 
      ([],[],[]),snd (add_free_list [lift_site sigma1 egfr1_r,[];
				     lift_site sigma2 egfr1_r,[]] remanent))
  (fun remanent -> 
    ([],[],[]),add_link_list [lift_site sigma1 egfr1_r,lift_site sigma2 egfr1_r] remanent)
    [Comment "$\assobis{}$"]
    
let _ = dump "dimerisation.ladot" [] remanent 
