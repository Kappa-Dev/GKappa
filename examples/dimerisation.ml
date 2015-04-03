(**
 * dimerisation.ml 
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

(** small example to drow a dimerisation rule *)

open Config
open Signature_egfr
  

let half_domain, 
  [
    egf1,[egf1_r,_];
    egfr1,[egfr1_l,_;egfr1_r,_]; 
  ]
  =
  add_in_graph 
    remanent 
    [
      egf,0.,0.,[],
      [egf_r,[Direction e],[]];
      egfr,2.5,0.,[],
      [egfr_l,[Direction w;Scale 0.9],[];
       egfr_r,[Direction s],[]]]

let half_domain = add_link_list [egfr1_l,egf1_r] half_domain
let other_half_domain = move_remanent_bellow 0.5 (horizontal_swap half_domain) half_domain  
 
let ag1,site1,state1,ag2,site2,state2,lhs_domain = disjoint_union half_domain other_half_domain 


let agl,sitel,statel,agr,siter,stater,domain = 
  disjoint_union lhs_domain (move_remanent_right_to 1. lhs_domain lhs_domain)

let _,rule_with_lhs = 
  add_free_list 
    [sitel (site1 egfr1_r),[];
     sitel (site2 egfr1_r),[]]
    domain 
(*let _,rule_with_lhs = add_free (sitel (site2 egfr1_l)) [] rule_with_lhs *)
let rule = add_link_list [siter (site1 egfr1_r),siter (site2 egfr1_r)] rule_with_lhs 

let x,x',y,y' = 
  match corners rule 
  with None -> 0.,0.,0.,0.
  | Some rep -> rep 
let rule = add_rule 4.3 (-.1.)  [Comment "k"] rule 

let _ = dump "dimerisation.dot" []  rule

