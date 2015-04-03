(**
 * egfr.ml 
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

(** Example to draw pictures related to the early egfr pathway *)

open Config
open Signature_egfr
 
let empty = Signature_egfr.remanent 




(* CONTACT MAP *)
let remanent, 
  [
    cm_egf,[cm_egf_r,_];
    cm_egfr,[cm_egfr_l,_;cm_egfr_r,_;cm_egfr_Y68,_;cm_egfr_Y48,_];
    cm_shc,[cm_shc_pi,_;cm_shc_Y7,_];
    cm_grb2,[cm_grb2_a,_;cm_grb2_b,_];
    cm_sos,[cm_sos_d,_]
  ]
  = 
  add_in_graph 
    empty 
    [
      egf,2.7,13.,[],
      [egf_r,[Direction s],
       [Free [Direction se]];] ;
      egfr,0.6,11.,[],
      [egfr_l,[Direction 30.;Scale 1.2],
       [Free [Direction e]];
       egfr_r,[Direction 150.;Scale 1.2],
       [Free [Direction s]];
       egfr_Y68,[Direction 205.;Scale 1.2],
       [Free [Direction sw];
	Internal (egfr_Y68_u,[Direction se]);
	Internal (egfr_Y68_p,[Direction (se+.20.)])];
       egfr_Y48,[Direction 330.;Scale 1.2],
       [Free [Direction ne];
	Internal (egfr_Y48_u,[Direction (w+.20.)]);
	Internal (egfr_Y48_p,[Direction w])]];
      shc,-.1.25,13.,[],
      [shc_pi,[Direction 110.],
       [Free []];
       shc_Y7,[Direction 250.],
       [Free [Direction sw];
	Internal (shc_Y7_u,[Direction (nw-.10.)]);
	Internal (shc_Y7_p,[Direction (nw-.30.)])]];    
      grb2,-.1.25,9.15,[],
       [grb2_a,[Direction nw],
	[Free [Direction (sw+.30.)]];
	grb2_b,[Direction e],
	[Free [Direction se]]];
      sos,2.7,9.15,[],
      [sos_d,[Direction w],
       [Free [Direction sw]]]]



let remanent = 
  add_link_list 
    [
      cm_egf_r,cm_egfr_l;
      cm_egfr_l,cm_egf_r;
      cm_egfr_r,cm_egfr_r;
      cm_egfr_Y48,cm_shc_pi;
      cm_shc_Y7,cm_grb2_a;
      cm_grb2_b,cm_sos_d;
      cm_egfr_Y68,cm_grb2_a]
    remanent 

let contact_map = remanent 


let add_flow_cm p remanent = 
  add_flow_list 
    [
      p cm_egf_r,p cm_egfr_l;
      p cm_egfr_l,p cm_egf_r;
      p cm_egfr_r,p cm_egfr_r;
      p cm_egfr_Y48,p cm_shc_pi;
      p cm_shc_Y7,p cm_grb2_a;
      p cm_grb2_b,p cm_sos_d;
      p cm_egfr_Y68,p cm_grb2_a;
      p cm_egfr_r,p cm_egfr_l;
      p cm_egfr_l,p cm_egfr_r;
      p cm_egfr_r,p cm_egfr_Y48;
      p cm_egfr_r,p cm_egfr_Y68;
      p cm_shc_pi,p cm_shc_Y7;
      p cm_shc_Y7,p cm_shc_pi;
      p cm_grb2_a,p cm_grb2_b;
      p cm_grb2_b,p cm_grb2_a
    ]
    remanent

let annotated_contact_map= add_flow_cm (fun x->x) contact_map
let _ = dump "contact_map.dot" []  contact_map
let _ = dump "contact_map_annotated.dot" []  annotated_contact_map 

let contact_map  = tag_all_nodes "contact_map" 1 contact_map

(*SPECIES*)

let remanent, 
  [
    sp_egf1,[sp_egf1_r,_];
    sp_egfr1,[sp_egfr1_l,_;sp_egfr1_r,_;sp_egfr1_Y68,_;sp_egfr1_Y48,_];
    sp_egf2,[sp_egf2_r,_];
    sp_egfr2,[sp_egfr2_l,_;sp_egfr2_r,_;sp_egfr2_Y68,_;sp_egfr2_Y48,_]  ;
    sp_shc1,[sp_shc1_pi,_;sp_shc1_Y7,_] ;
    sp_shc2,[sp_shc2_pi,_;sp_shc2_Y7,_] ;
    sp_grb21,[sp_grb21_a,_;sp_grb21_b,_] ;
    sp_grb22,[sp_grb22_a,_;sp_grb22_b,_] ;
    sp_sos1,[sp_sos1_d,_]  
  ] 
  =
  add_in_graph 
    empty 
    [
      egf,1.,13.8,[],
      [egf_r,[Direction s],[]];
      egfr,1.,12.,[],
      [egfr_l,[Direction n],[];
       egfr_r,[Direction e;Scale 0.9],[];
       egfr_Y68,[Direction s;Tag ("frag",1)],[Free ([Direction sw]);Internal (egfr_Y68_u,[Direction (se)])];
       egfr_Y48,[Direction w;Tag ("frag",2)],[Internal (egfr_Y48_p,[Direction 225.])]
      ];
      egf,3.5,12.8,[],
      [egf_r,[Direction s],[]];
      egfr,3.5,11.,[],
       [egfr_l,[Direction n],[];
	egfr_r,[Direction w;Scale 0.9],[];
	egfr_Y68,[Direction s;Tag ("frag",3)],[Internal (egfr_Y68_p,[Direction se])];
	egfr_Y48,[Direction e;Tag ("frag",4)],[Internal (egfr_Y48_p,[Direction e])]
       ];
      shc,-0.4,9.5,[Tag ("frag",2)],
      [shc_pi,[Direction n],[];
       shc_Y7,[Direction s],[Internal (shc_Y7_p,[Direction sw])];];
      shc,5.,13.,[Tag ("frag",4)],
      [shc_pi,[Direction s],[];
       shc_Y7,[Direction n],[Internal (shc_Y7_u,[Direction ne]);Free [Direction nw]]];
      grb2,1.8,8.,[Tag ("frag",2)],
      [grb2_a,[Direction w],[];
       grb2_b,[Direction e],[]];
      grb2,2.8,9.,[Tag ("frag",3)],
      [grb2_a,[Direction n],[];
       grb2_b,[Direction w],[Free [Direction sw]]];
      sos,4.5,8.,[Tag ("frag",2)],
      [sos_d,[Direction w],[]]]

let remanent = 
  add_link_list 
    [
      sp_egfr1_r,sp_egfr2_r;
      sp_egfr2_r,sp_egfr1_r;
      sp_egf2_r,sp_egfr2_l;
      sp_egf1_r,sp_egfr1_l;
      sp_egfr1_l,sp_egf1_r;
      sp_egfr2_l,sp_egf2_r;
      sp_egfr1_Y48,sp_shc1_pi;
      sp_egfr2_Y48,sp_shc2_pi;
      sp_shc1_Y7,sp_grb21_a;
      sp_grb21_b,sp_sos1_d;
      sp_egfr2_Y68,sp_grb22_a
    ]
    remanent 

let species = remanent 

let add_flow_species p remanent = 
   add_flow_list 
    [
      p sp_egfr1_r,p sp_egfr2_r;
      p sp_egfr2_r,p sp_egfr1_r;
      p sp_egf2_r,p sp_egfr2_l;
      p sp_egf1_r,p sp_egfr1_l;
      p sp_egfr1_l,p sp_egf1_r;
      p sp_egfr2_l,p sp_egf2_r;
      p sp_egfr1_Y48,p sp_shc1_pi;
      p sp_egfr2_Y48,p sp_shc2_pi;
      p sp_shc1_Y7,p sp_grb21_a;
      p sp_grb21_b,p sp_sos1_d;
      p sp_egfr2_Y68,p sp_grb22_a;
      p sp_egfr1_l,p sp_egfr1_r; 
      p sp_egfr1_r,p sp_egfr1_Y48;
      p sp_egfr1_r,p sp_egfr1_Y68;
      p sp_egfr2_l,p sp_egfr2_r;
      p sp_egfr2_r,p sp_egfr2_Y48;
      p sp_egfr2_r,p sp_egfr2_Y68;
      p sp_grb21_a,p sp_grb21_b;
      p sp_grb21_b,p sp_grb21_a;
      p sp_grb22_a,p sp_grb22_b;
      p sp_grb22_b,p sp_grb22_a;
      p sp_shc1_pi,p sp_shc1_Y7;
      p sp_shc1_Y7,p sp_shc1_pi;
      p sp_shc2_pi,p sp_shc2_Y7;
      p sp_shc2_Y7,p sp_shc2_pi]
     remanent 

let species_with_flow = add_flow_species (fun x -> x ) remanent 

let _ = dump "species.dot" ["flow",0]  species_with_flow
let _ = dump "species_annotated.dot" ["flow",1] species_with_flow
let _ = dump "species_frag1_annotated.dot" ["frag",1;"flow",1]  species_with_flow
let _ = dump "species_frag2_annotated.dot" ["frag",2;"flow",1]  species_with_flow
let _ = dump "species_frag3_annotated.dot" ["frag",3;"flow",1]  species_with_flow
let _ = dump "species_frag4_annotated.dot" ["frag",4;"flow",1]  species_with_flow
let _ = dump "species_frag1.dot" ["frag",1;"flow",0]  species_with_flow
let _ = dump "species_frag2.dot" ["frag",2;"flow",0]  species_with_flow
let _ = dump "species_frag3.dot" ["frag",3;"flow",0]  species_with_flow
let _ = dump "species_frag4.dot" ["frag",4;"flow",0]  species_with_flow 

(* SPECIES + CM *)

let trans_sp = translate (-9.) 0. species 
let trans_sp_with_flow = translate (-9.) 0. species_with_flow 
let ag_cm,site_cm,_,ag_sp,site_sp,_,trans_sp_with_flow_with_cm = disjoint_union contact_map trans_sp_with_flow 
let _ = dump "flow_annotated_species_cm.dot" [] trans_sp_with_flow_with_cm
let _ = dump "flow_annotated_species_annotated_cm.dot" []
  (add_flow_cm site_cm trans_sp_with_flow_with_cm)

let ag_cm,site_cm,_,ag_sp,site_sp,_,trans_sp_with_cm = disjoint_union contact_map trans_sp
let _ = dump "species_frag2_annotated_cm.dot" ["frag",2]
  (add_flow_cm site_cm trans_sp_with_cm)

let proj = 
   [
      ag_sp sp_grb22, ag_cm cm_grb2;
      ag_sp sp_grb21, ag_cm cm_grb2;
      ag_sp sp_egfr2,ag_cm cm_egfr;
      ag_sp sp_egfr1,ag_cm cm_egfr;
      ag_sp sp_egf2,ag_cm cm_egf;
      ag_sp sp_egf1,ag_cm cm_egf;
      ag_sp sp_shc2,ag_cm cm_shc;
      ag_sp sp_shc1,ag_cm cm_shc;
      ag_sp sp_sos1, ag_cm cm_sos] 

let proj_inv = List.map (fun (x,y) -> (y,x)) proj

let species_cm = 
  add_proj 
    proj 
    trans_sp_with_flow_with_cm

let _ = dump "flow_annotated_species_proj_cm.dot" [] species_cm 
let _ = dump "flow_annotated_species_proj_annotated_cm.dot" [] 
  (add_flow_cm site_cm species_cm) 


let _ = dump "species_frag2_proj_annotated_cm.dot" ["frag",2]
  (add_flow_cm site_cm (add_proj proj_inv trans_sp_with_cm))
let _ = dump "species_annotated_frag2_proj_annotated_cm.dot" ["frag",2;"flow",1]
  (add_flow_cm site_cm (add_proj proj_inv trans_sp_with_flow_with_cm))


(* RULE *)
let lhs_domain, 
  [
    rule_shc,[rule_shc_Y7,_;rule_shc_pi,_];
    rule_egfr,[rule_egfr_r,_;rule_egfr_Y48,_]; 
  ]
  =
  add_in_graph 
    empty 
    [
      shc,-5.,13.5,[],
      [shc_Y7,[Direction n],[Free [Direction n]];
       shc_pi,[Direction se],[]];
      egfr,-3.,12.,[],
      [egfr_r,[Direction s],[Bound [Direction s]];
      egfr_Y48,[Direction (nw+.5.);Scale 1.01],[]]]

let lhs_domain = add_link_list [rule_egfr_Y48,rule_shc_pi] lhs_domain 


let agl,sitel,statel,agr,siter,stater,domain = 
  disjoint_union lhs_domain (move_remanent_bellow 1. lhs_domain lhs_domain)

let site1 = (fun x -> x)
let site2 = (fun x -> x)

let _,domain = add_internal_state (sitel rule_shc_Y7) shc_Y7_u [Direction ne] domain 
let _,rule = add_internal_state (siter rule_shc_Y7) shc_Y7_p [Direction ne] domain 

let rule = add_rule (-.3.9)  10.1  [Direction s] rule 

let _ = dump "flow_rule.dot" ["contact_map",0;"flow",0]  rule 

let annotated_rule = 
  add_flow_list 
    [
      rule_egfr_r,rule_egfr_Y48;
      rule_egfr_Y48,rule_shc_pi;
      rule_shc_pi,rule_shc_Y7
    ]
    rule 

let _ = dump "flow_annotated_rule.dot" [] annotated_rule 

(* RULE + CM *) 

let rule_plus_x x string proj_list add_flow = 
  let _,sitel,_,_,_,_,all = disjoint_union x (translate (-3.) 0. rule) in 
  let _ = dump ("flow_rule_"^string^".dot") [] all in 
  let _ = dump ("flow_rule_annotated_"^string^".dot") [] (add_flow sitel all) in 
  let agl,sitel,_,agr,siter_,_,annotated_rule_x = disjoint_union x (translate (-.3.) 0. annotated_rule) in 
  let _ = dump ("flow_annotated_rule_"^string^".dot") [] annotated_rule_x  in 
  let proj_annotated_rule_x = 
    add_proj 
      (proj_list agr agl)
      annotated_rule_x
  in 
  let _ = dump ("flow_annotated_rule_proj_"^string^".dot") [] proj_annotated_rule_x in 
  let annotated_rule_annotated_x = add_flow sitel annotated_rule_x in 
  let _ = 
    dump ("flow_annotated_rule_annotated_"^string^".dot") [] annotated_rule_annotated_x 
  in 
  let annotated_rule_proj_annotated_x = add_flow sitel proj_annotated_rule_x in 
  dump ("flow_annotated_rule_proj_annotated_"^string^".dot") [] annotated_rule_proj_annotated_x 


let proj_list_cm agr agl = 
  [
    agr rule_shc,agl cm_shc;
    agr rule_egfr, agl cm_egfr] 

let proj_list_sp agr agl = 
  [agr rule_shc,agl sp_shc2;
   agr rule_egfr,agl sp_egfr2]

let add_flow_cm sitel  = 
  add_flow_list 
    [ 
      sitel cm_egfr_r,sitel cm_egfr_Y48;
      sitel cm_egfr_Y48,sitel cm_shc_pi;
      sitel cm_shc_pi,sitel cm_shc_Y7
    ]

let add_flow_species silel = 
  add_flow_list 
    [
      sitel sp_egfr2_r,sitel sp_egfr2_Y48;
      sitel sp_egfr2_Y48,sitel sp_shc2_pi;
      sitel sp_shc2_pi,sitel sp_shc2_Y7
    ]
    
let _ = rule_plus_x contact_map "cm" proj_list_cm add_flow_cm 
let _ = rule_plus_x (vertical_swap species) "sp" proj_list_sp add_flow_species

(* frag3 + CM *)


let exaa = [Tag ("Exab",0);Tag("Exac",0);Tag("Exad",0)]

let short_species, 
  [
    sp_egf1,[sp_egf1_r,_];
    sp_egfr1,[sp_egfr1_l,_;sp_egfr1_r,_;sp_egfr1_Y68,_;sp_egfr1_Y48,_];
    sp_egf2,[sp_egf2_r,_];
    sp_egfr2,[sp_egfr2_l,_;sp_egfr2_r,_;sp_egfr2_Y68,_]  ;
  ] 
  =
  add_in_graph 
    empty 
    [
      egf,1.,13.8,[],
      [egf_r,[Direction s],[]];
      egfr,1.,12.,[],
      [egfr_l,[Direction n],[];
       egfr_r,[Direction e;Scale 0.9],[];
       egfr_Y68,[Direction s;Tag ("Exaa",0);Tag ("Exab",0);Tag("Exac",0)],[Free ([Direction sw]);Internal (egfr_Y68_u,[Direction (se)])];
       egfr_Y48,[Direction w;Tag ("Exad",0);Tag ("Exac",0)],[Internal (egfr_Y48_p,[Direction 225.])]
      ];
      egf,3.5,13.8,[],
      [egf_r,[Direction s],[]];
      egfr,3.5,12.,[],
       [egfr_l,[Direction n],[];
	egfr_r,[Direction w;Scale 0.9],[];
	egfr_Y68,[Direction s;Tag ("Exaa",0);Tag ("Exad",0);Tag("Exac",0)],[Internal (egfr_Y68_p,[Direction se])];
       ]]


let short_species = 
  add_link_list 
    [
      sp_egfr1_r,sp_egfr2_r;
      sp_egf2_r,sp_egfr2_l;
      sp_egf1_r,sp_egfr1_l;
    ]
    short_species

let short_species =
   add_flow_list 
    [
      sp_egfr1_r,sp_egfr2_r;
      sp_egfr2_r,sp_egfr1_r;
      sp_egf2_r,sp_egfr2_l;
      sp_egf1_r,sp_egfr1_l;
      sp_egfr1_l,sp_egf1_r;
      sp_egfr2_l,sp_egf2_r;
      sp_egfr1_l,sp_egfr1_r; 
      sp_egfr1_r,sp_egfr1_Y48;
      sp_egfr1_r,sp_egfr1_Y68;
      sp_egfr2_l,sp_egfr2_r;
      sp_egfr2_r,sp_egfr2_Y68
    ]
     short_species

let _ = dump "short_species_exa_annotated.dot" ["Exaa",1]  short_species 
let _ = dump "short_species_exa.dot" ["Exaa",1;"flow",0]  short_species 
let _ = dump "short_species_exb_annotated.dot" ["Exab",1]  short_species 
let _ = dump "short_species_exb.dot" ["Exab",1;"flow",0]  short_species 
let _ = dump "short_species_exc_annotated.dot" ["Exac",1]  short_species 
let _ = dump "short_species_exc.dot" ["Exac",1;"flow",0]  short_species 
let _ = dump "short_species_exd_annotated.dot" ["Exad",1]  short_species 
let _ = dump "short_species_exd.dot" ["Exad",1;"flow",0]  short_species 
let _ = dump "short_species_exb_annotated_crossed.dot" ["Exab",1]  (cross short_species)
let _ = dump "short_species_exb_crossed.dot" ["Exab",1;"flow",0]  (cross short_species )
let _ = dump "short_species_exc_annotated_crossed.dot" ["Exac",1]  (cross short_species)
let _ = dump "short_species_exc_crossed.dot" ["Exac",1;"flow",0]  (cross short_species )


(* FRAG CONS / PROD  *)
let lhs_domain, 
  [
    cons_shc,[cons_shc_Y7,_;cons_shc_pi,_];
    cons_egfr,[cons_egfr_r,_;cons_egfr_Y48,_]; 
  ]
  =
  add_in_graph 
    empty 
    [
      shc,-5.,12.,[],
      [shc_Y7,[Direction w],[Bound[Direction nw]];
       shc_pi,[Direction e],[]];
      egfr,-3.,12.,[],
      [egfr_r,[Direction e],[Bound [Direction se]];
      egfr_Y48,[Direction w],[]]]

let lhs_domain = add_link_list [cons_egfr_Y48,cons_shc_pi] lhs_domain 


let agl,sitel,statel,agr,siter,stater,domain = 
  disjoint_union lhs_domain (move_remanent_right_to 1.2 lhs_domain lhs_domain)

let site1 = (fun x -> x)
let site2 = (fun x -> x)

let _,rule = add_internal_state (sitel cons_shc_Y7) shc_Y7_u [Direction sw] domain 
let _,rule = add_internal_state (siter cons_shc_Y7) shc_Y7_p [Direction sw] rule

let rule = add_rule  (-.1.)  12.  [Direction e] rule 

let _ = dump "flow_cons_rule.dot" ["contact_map",0;"flow",0]  rule 

let (Some cons_egfr_l),fragment = add_site cons_egfr egfr_l [Direction s] lhs_domain 
let _,fragment = add_bound cons_egfr_l [Direction se] fragment 

let _ = dump "flow_cons.dot" ["contact_map",0;"flow",0]  fragment 

let agrule,siter,stater,agf,sitef,statef,cons =   disjoint_union rule (move_remanent_bellow 0. fragment lhs_domain)

let cons1 = add_match
  [ agrule (agl cons_egfr), agf cons_egfr;
    agrule (agl cons_shc),agf cons_shc]
      cons 

let cons2 = add_proj 
  [ agrule (agl cons_egfr), agf cons_egfr;
    agrule (agl cons_shc),agf cons_shc]
  cons 

let _ = dump "flow_cons_match.dot" ["contact_map",0;"flow",0]  cons1
let _ = dump "flow_cons_proj.dot" ["contact_map",0;"flow",0]  cons2


let fragment, 
  [
    frag_shc,[frag_shc_Y7,_;frag_shc_pi,_];
    frag_egfr,[frag_egfr_l,_;frag_egfr_Y48,_]; 
  ]
  =
  add_in_graph 
    empty 
    [
      shc,-5.,12.,[],
      [shc_Y7,[Direction w],[Bound[Direction nw]];
       shc_pi,[Direction e],[]];
      egfr,-3.,12.,[],
      [egfr_l,[Direction s],[Bound [Direction se]];
      egfr_Y48,[Direction w],[]]]

let _,fragment = add_bound frag_egfr_l [Direction se] fragment 

let agrule,siterule,staterule,agf,sitef,statef,prod =   disjoint_union rule (move_remanent_bellow 0. (translate 6. 0. fragment) lhs_domain)


let prod1 = add_match
  [ agrule (agr cons_egfr), agf frag_egfr;
    agrule (agr cons_shc),agf frag_shc]
      prod
let Some st,prod2 = add_site (agrule (agr cons_egfr)) egfr_l [Direction s;Color "red"] prod1 
let _,prod2 = add_bound st [Color "red"] prod2 
let Some st,prod2 = add_site (agrule (agl cons_egfr)) egfr_l [Direction s;Color "red"] prod2
let _,prod2 = add_bound st [Color "red"] prod2 
let _ = dump "flow_prod_match.dot" ["contact_map",0;"flow",0]  prod1
let _ = dump "flow_prod_overlap.dot" ["contact_map",0;"flow",0]  prod2

let short_species, 
  [
    sp_egf1,[sp_egf1_r,_];
    sp_egfr1,[sp_egfr1_l,_;sp_egfr1_r,_;sp_egfr1_Y68,_;sp_egfr1_Y48,_];
    sp_egf2,[sp_egf2_r,_];
    sp_egfr2,[sp_egfr2_l,_;sp_egfr2_r,_;sp_egfr2_Y68,_;sp_egfr2_Y48,_];
    sp_shc,[sp_shc_pi,_;sp_shc_Y7,_]]
  =
  add_in_graph 
    empty 
    [
      egf,1.,13.8,[Tag ("species",1)],
      [	egf_r,[Direction s],[]];
      egfr,1.,12.,[Tag ("species",1)],
      [egfr_l,[Direction n],[];
       egfr_r,[Direction e;Scale 0.9],[];
       egfr_Y68,[Direction s;Tag ("Exaa",0);Tag ("Exab",0);Tag("Exac",0)],[Free ([Direction sw]);Internal (egfr_Y68_u,[Direction (se)])];
       egfr_Y48,[Direction w;Tag ("Exad",0);Tag ("Exac",0)],[Internal (egfr_Y48_p,[Direction 225.])]
      ];
      egf,3.5,13.8,[],
      [egf_r,[Direction s],[]];
       egfr,3.5,12.,[],
       [egfr_l,[Direction n],[];
	egfr_r,[Direction w;Scale 0.9],[];
	egfr_Y68,[Direction e;Tag ("Exaa",0);Tag ("Exad",0);Tag("Exac",0)],[Internal (egfr_Y68_p,[Direction se])];
	egfr_Y48,[Direction s],[]
       ];
       shc,3.5,10.2,[],
      [shc_pi,[Direction n],[];
       shc_Y7,[Direction s],[Internal (shc_Y7_u,[Direction se]);Free [Direction s]]]]
      


let short_species = 
  add_link_list 
    [
      sp_egfr1_r,sp_egfr2_r;
      sp_egf2_r,sp_egfr2_l;
      sp_egf1_r,sp_egfr1_l;
      sp_egfr2_Y48,sp_shc_pi
    ]
    short_species

let _ = dump "flow_species.dot" []  short_species
let _ = dump "flow_species_sp.dot" ["species",0]  (snd (add_free sp_egfr2_r [] short_species))

let short_species = 
  add_flow_list 
    [sp_egfr2_r,sp_egfr2_Y48;
    sp_egfr2_Y48,sp_shc_pi;
    sp_shc_pi,sp_shc_Y7]
    short_species

 
let _ = dump "flow_species_annotated.dot" []  short_species
let _ = dump "flow_species_sp_annotated.dot" ["species",0]  (snd (add_free sp_egfr2_r [] short_species))

let lhs_domain, 
  [
    cons_shc,[cons_shc_Y7,_;cons_shc_pi,_];
    cons_egfr,[cons_egfr_r,_;cons_egfr_Y48,_]; 
  ]
  =
  add_in_graph 
    empty 
    [
      shc,-5.,12.,[],
      [shc_Y7,[Direction w],[Free[Direction nw]];
       shc_pi,[Direction e],[]];
      egfr,-3.,12.,[],
      [egfr_r,[Direction e],[Bound [Direction se]];
      egfr_Y48,[Direction w],[]]]

let lhs_domain = add_link_list [cons_egfr_Y48,cons_shc_pi] lhs_domain 


let agl,sitel,statel,agr,siter,stater,domain = 
  disjoint_union lhs_domain (move_remanent_right_to 1.2 lhs_domain lhs_domain)

let site1 = (fun x -> x)
let site2 = (fun x -> x)

let _,rule = add_internal_state (sitel cons_shc_Y7) shc_Y7_u [Direction sw] domain 
let _,rule = add_internal_state (siter cons_shc_Y7) shc_Y7_p [Direction sw] rule

let rule = add_rule  (-.1.)  12.  [Direction e] rule 

let _ = dump "flow_cons_rule.dot" ["contact_map",0;"flow",0]  rule 
