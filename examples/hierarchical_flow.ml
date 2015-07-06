(**
 * hierarchical_flow.ml 
 * GKappa
 * Jérôme Feret, projet Antique, INRIA Paris-Rocquencourt
 * 
 * Creation:                      <2015-03-28 feret>
 * Last modification: Time-stamp: <2015-07-05 14:09:13 feret>
 * * 
 *  
 * Copyright 2015 Institut National de Recherche en Informatique  * et en Automatique.  All rights reserved.  
 * This file is distributed under the terms of the 
 * GNU Library General Public License *)


(** GKappa is a OCAML library to help making slides in Kappa *)
(** More applications are coming soon (hopefully) *)

(** Example that draws the pictures for the model: 

  A(y~u) -> A(y~p)
  A(y~p,x~u) -> A(y~p,x~p)
  A(y~p,z~u) -> A(y~p,z~p)
*)

open Config
open Geometry 

(* chemical species*)
let _,remanent = init config 
(*signature*)
let 
  [p,
   [
     g,[gu;gp]; 
     c,[cu;cp];
     d,[du;dp]
   ]
  ],
  remanent 
    = add_in_signature 
  [
    "",[FillColor "white";Width 0.8;Height 0.8;Shape "rectangle"],
    [
      "g",[FillColor "green";Radius 0.4;Direction sw],["gu",[];"gp",[]];
      "c",[FillColor "red";Radius 0.4;Direction n],["cu",[];"cp",[]];
      "d",[FillColor "blue";Radius 0.4;Direction se],["du",[];"dp",[]]
    ]
  ]
  remanent 

(* CONTACT MAP *)
let [
    cm_p,[cm_c,[cm_cu;cm_cp];
	  cm_g,[cm_gu;cm_gp];
	  cm_d,[cm_du;cm_dp]]
  ],
  contact_map
  = 
  add_in_graph 
    [
      p,0.,0.,[],
      [c,[],
       [Internal_state (cu,[Direction (of_degree (to_degree n-.15.))]);
        Internal_state (cp,[Direction (of_degree (to_degree n+.15.))])];
       g,[Tag ("site",0)],
       [Internal_state (gu,[Direction (of_degree (to_degree sw-.15.))]);
	Internal_state (gp,[Direction (of_degree (to_degree sw+.15.))])];
       d,[Tag ("site",1)],
       [Internal_state (gu,[Direction (of_degree (to_degree se+.15.))]);
	Internal_state (gp,[Direction (of_degree (to_degree se-.15.))])]]]
    remanent 

let annotated_contact_map = 
  add_flow_list 
    [
      cm_c,cm_d;
      cm_c,cm_g
    ]
    contact_map

let _ = dump "hier_contact_map.dot" contact_map
let _ = dump "hier_contact_map_annotated.dot" annotated_contact_map
let _ = dump "hier_contact_map_d_annotated.dot" ~flags:["site",1]  annotated_contact_map 
let _ = dump "hier_contact_map_g_annotated.dot" ~flags:["site",0]  annotated_contact_map

let p,nnn = add_agent p 0. 0. remanent
let sg,unn = add_site p g nnn
let _,pnn = add_internal_state sg gp ~directives:[Direction sw;Scale 1.2] unn
let _,unn = add_internal_state sg gu ~directives:[Direction sw;Scale 1.2] unn 
let sc,nun = add_site p c nnn
let _,npn = add_internal_state sc cp ~directives:[Direction n] nun
let _,nun = add_internal_state sc cu ~directives:[Direction n] nun 
let sd,nnu = add_site p d nnn
let _,nnp = add_internal_state sd dp ~directives:[Direction se;Scale 1.2] nnu
let _,nnu = add_internal_state sd du ~directives:[Direction se;Scale 1.2] nnu
let sg,spn = add_site p g npn 
let _,ppn = add_internal_state sg gp ~directives:[Direction sw;Scale 1.2] spn
let _,upn = add_internal_state sg gu ~directives:[Direction sw;Scale 1.2] spn 
let sd,nps = add_site p d npn 
let _,npp = add_internal_state sd dp ~directives:[Direction se] nps
let _,npu = add_internal_state sd du ~directives:[Direction se] nps 
let sg,spu = add_site p g npu 
let _,ppu = add_internal_state sg gp ~directives:[Direction sw] spu
let _,upu = add_internal_state sg gu ~directives:[Direction sw] spu 
let sg,spp = add_site p g npp 
let _,ppp = add_internal_state sg gp ~directives:[Direction sw] spp
let _,upp = add_internal_state sg gu ~directives:[Direction sw] spp 
let sg,sun = add_site p g nun 
let _,uun = add_internal_state sg gu ~directives:[Direction sw] sun 
let sd,uus = add_site p d uun
let _,uuu = add_internal_state sd du ~directives:[Direction se] uus


 
let _ = dump "hier_uuu.dot" uuu
let _ = dump "hier_upu.dot" upu 
let _ = dump "hier_ppp.dot" ppp
let _ = dump "hier_upp.dot" upp
let _ = dump "hier_ppu.dot" ppu
let _ = dump "hier_upn.dot" upn 
let _ = dump "hier_ppn.dot" ppn
let _ = dump "hier_npp.dot" npp
let _ = dump "hier_npu.dot" npu

let _,_,tmp =  
  disjoint_union uuu 
    (move_remanent_right_to 0.6 upu upu)

let reaction_c = 
  translate_graph {abscisse = 2. ; ordinate =  0.} (add_rule 1.1 0. ~directives:[Comment("$\ky$");Height 1.2] tmp)


let _,_,tmp =  
  disjoint_union upu 
    (move_remanent_right_to 0.6 upp upp)
let reaction_du = add_rule 1.1 0. ~directives:[Comment("$\kz$");Height 0.6] tmp 
let _,_,tmp = disjoint_union ppu (move_remanent_right_to 0.6 ppp ppp)
let reaction_dp = add_rule 1.1 0. ~directives:[Comment("$\kz$");Height 0.6] tmp 
let _,_,tmp = disjoint_union upu (move_remanent_right_to 0.6 ppu ppu)

let reaction_gu = add_rule 1.1 0. ~directives:[Comment("$\kx$");Height 0.6] tmp 
let _,_,tmp = disjoint_union upp (move_remanent_right_to 0.6 ppp ppp)
let reaction_gp = add_rule 1.1 0. ~directives:[Comment("$\kx$");Height 0.6] tmp 


let _ = dump "hier_reaction_c.ladot" reaction_c
let _ = dump "hier_reaction_du.ladot" reaction_du
let _ = dump "hier_reaction_dp.ladot" reaction_dp
let _ = dump "hier_reaction_gu.ladot" reaction_gu
let _ = dump "hier_reaction_gp.ladot" reaction_gp

let _,_,reaction_d = disjoint_union reaction_du (move_remanent_right_to 1. reaction_dp reaction_du)
let _,_,reaction_g = disjoint_union reaction_gu (move_remanent_right_to 1. reaction_gp reaction_gu)
let _ = dump "hier_reaction_d.ladot" reaction_d
let _ = dump "hier_reaction_g.ladot" reaction_g

let _,_,reactions = disjoint_union reaction_c (move_remanent_bellow 0. reaction_d reaction_c)

let _,_,reactions = disjoint_union reactions (move_remanent_bellow 0. reaction_g reactions)

let _ = dump "hier_reactions.ladot" reactions

let _,_,tmp = disjoint_union upn (move_remanent_right_to 0.75 ppn ppn)
let reaction_gn = add_rule 1.3 0. ~directives:[Comment("$\kx$");Height 0.6] tmp 
let _,_,tmp = disjoint_union npu (move_remanent_right_to 0.6 npp npp)
let reaction_dn = add_rule 1.3 0. ~directives:[Comment("$\kz$");Height 0.6] tmp 
let _ = dump "hier_reaction_gn.ladot" reaction_gn
let _ = dump "hier_reaction_dn.ladot" reaction_dn
 

