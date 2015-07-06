(**
 * signature_egfr.ml 
 * GKappa
 * Jérôme Feret, projet Antique, INRIA Paris-Rocquencourt
 * 
 * Creation: March, the 28th of 2015
 * Last modification: Time-stamp: <2015-07-05 10:00:35 feret>
 * * 
 *  
 * Copyright 2015 Institut National de Recherche en Informatique  * et en Automatique.  All rights reserved.  
 * This file is distributed under the terms of the 
 * GNU Library General Public License *)


(** GKappa is a OCAML library to help making slides in Kappa *)
(** More applications are coming soon (hopefully) *)

(** signature of the early egfr model *)
open Geometry 
open Gkappa

(* chemical species*)
let _,init = init Config.config 
(*signature*)
let  
    [
      egf,
      [egf_r,[]];
      egfr,
      [egfr_l,[];
       egfr_r,[];
       egfr_Y48,[egfr_Y48_u;egfr_Y48_p];
       egfr_Y68,[egfr_Y68_u;egfr_Y68_p]];
      shc,
      [shc_pi,[];
       shc_Y7,[shc_Y7_u;shc_Y7_p]];
      grb2,
      [grb2_a,[];
       grb2_b,[]];
      sos,
      [sos_d,[]]],
  signature_egfr 
  = 
  add_in_signature  
    [
      "EGF",[Width 1.2;Height 0.8;Shape "ellipse"],
      [
	"r",[Direction s],[]
      ];
      "EGFR",[Width 1.5;Height 0.8;Shape "hexagon"],
      [
	"l",[],[];
	"r",[],[];
	"Y48",[],["u",[];"p",[]];
	"Y68",[],["u",[];"p",[]]
      ];
      "ShC",[Width 1.;Height 1.;Shape "ellipse"],
      [
	"pi",[],[];
	"Y7",[],["u",[];"p",[]]
      ];
      "Grb2",[Width 1.5;Height 0.8;Shape "ellipse"],
      [
	"a",[],[];
	"b",[],[]
      ];
      "Sos",[Width 1.2;Height 0.8;Shape "ellipse"],
      [
	"d",[],[]
      ]]
    init 
