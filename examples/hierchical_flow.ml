(**
 * hierarchical_flow.ml 
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

(** Example that draws the pictures for the model: 

  A(y~u) -> A(y~p)
  A(y~p,x~u) -> A(y~p,x~p)
  A(y~p,z~u) -> A(y~p,z~p)
*)

open Config 



(* chemical species*)
let _,remanent = init Config.config 
(*signature*)
let p,remanent = add_agent_type "" [FillColor "white";Width 0.8;Height 0.8;Shape "rectangle"] remanent 
let g,remanent = add_site_type p "g" [FillColor "green";Radius 0.4;Direction sw;Scale 1.2] remanent 
let c,remanent = add_site_type p "c" [FillColor "red";Radius 0.4;Direction n] remanent 
let d,remanent = add_site_type p "d" [FillColor "blue";Radius 0.4;Direction se;Scale 1.2] remanent 
let gu,remanent = add_internal_state_type g "u" [] remanent 
let gp,remanent = add_internal_state_type g "p" [] remanent 
let cu,remanent = add_internal_state_type c "u" [] remanent 
let cp,remanent = add_internal_state_type c "p" [] remanent 
let du,remanent = add_internal_state_type d "u" [] remanent 
let dp,remanent = add_internal_state_type d "p" [] remanent 


(* CONTACT MAP *)
let contact_map , 
  [
    cm_p,[cm_c,[cm_cu;cm_cp];
	  cm_g,[cm_gu;cm_gp];
	  cm_d,[cm_du;cm_dp]]
  ]
  = 
  add_in_graph 
    remanent 
    [
      p,0.,0.,[],
      [c,[],
       [Internal (cu,[Direction (n-.15.)]);
        Internal (cp,[Direction (n+.15.)])];
       g,[Tag ("site",0)],
       [Internal (gu,[Direction (sw-.15.)]);
	Internal (gp,[Direction (sw+.15.)])];
       d,[Tag ("site",1)],
       [Internal (gu,[Direction (se+.15.)]);
	Internal (gp,[Direction (se-.15.)])]]]

let annotated_contact_map = 
  add_flow_list 
    [
      cm_c,cm_d;
      cm_c,cm_g
    ]
    contact_map

let _ = dump "hier_contact_map.dot" []  contact_map
let _ = dump "hier_contact_map_annotated.dot" []  annotated_contact_map
let _ = dump "hier_contact_map_d_annotated.dot" ["site",1]  annotated_contact_map 
let _ = dump "hier_contact_map_g_annotated.dot" ["site",0]  annotated_contact_map

let p,nnn = add_agent p 0. 0. [] remanent
let sg,unn = add_site p g [] nnn
let _,pnn = add_internal_state sg gp [Direction sw;Scale 1.2] unn
let _,unn = add_internal_state sg gu [Direction sw;Scale 1.2] unn 
let sc,nun = add_site p c [] nnn
let _,npn = add_internal_state sc cp [Direction n] nun
let _,nun = add_internal_state sc cu [Direction n] nun 
let sd,nnu = add_site p d [] nnn
let _,nnp = add_internal_state sd dp [Direction se;Scale 1.2] nnu
let _,nnu = add_internal_state sd du [Direction se;Scale 1.2] nnu
let sg,spn = add_site p g [] npn 
let _,ppn = add_internal_state sg gp [Direction sw;Scale 1.2] spn
let _,upn = add_internal_state sg gu [Direction sw;Scale 1.2] spn 
let sd,nps = add_site p d [] npn 
let _,npp = add_internal_state sd dp [Direction se] nps
let _,npu = add_internal_state sd du [Direction se] nps 
let sg,spu = add_site p g [] npu 
let _,ppu = add_internal_state sg gp [Direction sw] spu
let _,upu = add_internal_state sg gu [Direction sw] spu 
let sg,spp = add_site p g [] npp 
let _,ppp = add_internal_state sg gp [Direction sw] spp
let _,upp = add_internal_state sg gu [Direction sw] spp 
let sg,sun = add_site p g [] nun 
let _,uun = add_internal_state sg gu [Direction sw] sun 
let sd,uus = add_site p d [] uun
let _,uuu = add_internal_state sd du [Direction se] uus


 
let _ = dump "hier_uuu.dot" []  uuu
let _ = dump "hier_upu.dot" []  upu 
let _ = dump "hier_ppp.dot" []  ppp
let _ = dump "hier_upp.dot" []  upp
let _ = dump "hier_ppu.dot" []  ppu
let _ = dump "hier_upn.dot" []  upn 
let _ = dump "hier_ppn.dot" []  ppn
let _ = dump "hier_npp.dot" []  npp
let _ = dump "hier_npu.dot" []  npu

let _,_,_,_,_,_,tmp =  
  disjoint_union uuu 
    (move_remanent_right_to 0.6 upu upu)
let reaction_c = 
  add_rule 1.3 0. 
    [Comment("kc");Height 0.6] tmp 

let _,_,_,_,_,_,tmp =  
  disjoint_union upu 
    (move_remanent_right_to 0.6 upp upp)
let reaction_du = 
  add_rule 1.3 0. 
    [Comment("kd");Height 0.6] tmp 
let _,_,_,_,_,_,tmp =  
  disjoint_union ppu 
    (move_remanent_right_to 0.6 ppp ppp)
let reaction_dp = 
  add_rule 1.3 0. 
    [Comment("kd");Height 0.6] tmp 
let _,_,_,_,_,_,tmp =  
  disjoint_union upu 
    (move_remanent_right_to 0.6 ppu ppu)

let reaction_gu = 
  add_rule 1.3 0. 
    [Comment("kd");Height 0.6] tmp 
let _,_,_,_,_,_,tmp =  
  disjoint_union upp
    (move_remanent_right_to 0.6 ppp ppp)
let reaction_gp = 
  add_rule 1.3 0. 
    [Comment("kd");Height 0.6] tmp 


let _ = dump "hier_reaction_c.dot" [] reaction_c
let _ = dump "hier_reaction_du.dot" [] reaction_du
let _ = dump "hier_reaction_dp.dot" [] reaction_dp
let _ = dump "hier_reaction_gu.dot" [] reaction_gu
let _ = dump "hier_reaction_gp.dot" [] reaction_gp

let _,_,_,_,_,_,tmp =  
  disjoint_union upn
    (move_remanent_right_to 0.75 ppn ppn)
let reaction_gn = 
  add_rule 1.3 0. 
    [Comment("kg");Height 0.6] tmp 
let _,_,_,_,_,_,tmp =  
  disjoint_union npu
    (move_remanent_right_to 0.6 npp npp)
let reaction_dn = 
  add_rule 1.3 0. 
    [Comment("kd");Height 0.6] tmp 
let _ = dump "hier_reaction_gn.dot" [] reaction_gn
let _ = dump "hier_reaction_dn.dot" [] reaction_dn
 

