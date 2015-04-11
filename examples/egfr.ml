
(**
 * egfr.ml 
 * GKappa
 * Jérôme Feret, projet Antique, INRIA Paris-Rocquencourt
 * 
 * Creation:                      <2015-03-28 feret>
 * Last modification: Time-stamp: <2015-04-11 21:52:12 feret>
 * * 
 *  
 * Copyright 2015 Institut National de Recherche en Informatique  * et en Automatique.  All rights reserved.  
 * This file is distributed under the terms of the 
 * GNU Library General Public License *)


(** GKappa is a OCAML library to help making slides in Kappa *)
(** More applications are coming soon (hopefully) *)

(** Example to draw pictures related to the early egfr pathway *)

open Config
open Geometry 
open Signature_egfr
 
let empty = Signature_egfr.signature_egfr 




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
       [Free_site [Direction se]];] ;
      egfr,0.6,11.,[],
      [egfr_l,[Direction (of_degree 30.);Scale 1.2],
       [Free_site [Direction e]];
       egfr_r,[Direction (of_degree 150.);Scale 1.2],
       [Free_site [Direction s]];
       egfr_Y68,[Direction (of_degree 205.);Scale 1.2],
       [Free_site [Direction (of_degree (to_degree sw-.10.))];
	Internal_state (egfr_Y68_u,[Direction se]);
	Internal_state (egfr_Y68_p,[Direction (of_degree (to_degree se+.20.))])];
       egfr_Y48,[Direction (of_degree 330.);Scale 1.2],
       [Free_site [Direction ne];
	Internal_state (egfr_Y48_u,[Direction (of_degree (to_degree w+.20.))]);
	Internal_state (egfr_Y48_p,[Direction w])]];
      shc,-.1.25,13.,[],
      [shc_pi,[Direction (of_degree 110.)],
       [Free_site []];
       shc_Y7,[Direction (of_degree 250.)],
       [Free_site [Direction sw];
	Internal_state (shc_Y7_u,[Direction (of_degree (to_degree nw-.10.))]);
	Internal_state (shc_Y7_p,[Direction (of_degree (to_degree nw-.30.))])]];    
      grb2,-.1.25,9.15,[],
       [grb2_a,[Direction nw],
	[Free_site [Direction (of_degree (to_degree sw+.30.))]];
	grb2_b,[Direction e],
	[Free_site [Direction se]]];
      sos,2.7,9.15,[],
      [sos_d,[Direction w],
       [Free_site [Direction sw]]]]



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
       egfr_Y68,[Direction s;Tag ("frag",1)],[Free_site ([Direction sw]);Internal_state (egfr_Y68_u,[Direction (se)])];
       egfr_Y48,[Direction w;Tag ("frag",2)],[Internal_state (egfr_Y48_p,[Direction (of_degree 225.)])]
      ];
      egf,3.5,12.8,[],
      [egf_r,[Direction s],[]];
      egfr,3.5,11.,[],
       [egfr_l,[Direction n],[];
	egfr_r,[Direction w;Scale 0.9],[];
	egfr_Y68,[Direction s;Tag ("frag",3)],[Internal_state (egfr_Y68_p,[Direction se])];
	egfr_Y48,[Direction e;Tag ("frag",4)],[Internal_state (egfr_Y48_p,[Direction e])]
       ];
      shc,-0.4,9.5,[Tag ("frag",2)],
      [shc_pi,[Direction n],[];
       shc_Y7,[Direction s],[Internal_state (shc_Y7_p,[Direction sw])];];
      shc,5.,13.,[Tag ("frag",4)],
      [shc_pi,[Direction s],[];
       shc_Y7,[Direction n],[Internal_state (shc_Y7_u,[Direction ne]);Free_site [Direction nw]]];
      grb2,1.8,8.,[Tag ("frag",2)],
      [grb2_a,[Direction w],[];
       grb2_b,[Direction e],[]];
      grb2,2.8,9.,[Tag ("frag",3)],
      [grb2_a,[Direction n],[];
       grb2_b,[Direction w],[Free_site [Direction sw]]];
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

let trans_sp = translate_graph {abscisse = -9.;ordinate =  0.} species 
let trans_sp_with_flow = translate_graph {abscisse = -9.;ordinate =  0.} species_with_flow 
let sigma_cm,sigma_sp,trans_sp_with_flow_with_cm = disjoint_union contact_map trans_sp_with_flow 
let _ = dump "flow_annotated_species_cm.dot" [] trans_sp_with_flow_with_cm
let _ = dump "flow_annotated_species_annotated_cm.dot" []
  (add_flow_cm (lift_site sigma_cm) trans_sp_with_flow_with_cm)

let sigma_cm,sigma_sp,trans_sp_with_cm = disjoint_union contact_map trans_sp
let _ = dump "species_frag2_annotated_cm.dot" ["frag",2]
  (add_flow_cm (lift_site sigma_cm) trans_sp_with_cm)


let proj = 
  List.rev_map 
    (fun (x,y) -> lift_agent sigma_sp x,lift_agent sigma_cm y)
    [sp_grb22,cm_grb2;
     sp_grb21,cm_grb2;
     sp_egfr2,cm_egfr;
     sp_egfr1,cm_egfr;
     sp_egf2,cm_egf;
     sp_egf1,cm_egf;
     sp_shc2,cm_shc;
     sp_shc1,cm_shc;
     sp_sos1,cm_sos] 

let proj_inv = List.map (fun (x,y) -> (y,x)) proj

let species_cm = 
  add_proj 
    proj 
    trans_sp_with_flow_with_cm

let _ = dump "flow_annotated_species_proj_cm.dot" [] species_cm 
let _ = dump "flow_annotated_species_proj_annotated_cm.dot" [] 
  (add_flow_cm (lift_site sigma_cm) species_cm) 



let _ = dump "species_frag2_proj_annotated_cm.dot" ["frag",2]
  (add_flow_cm (lift_site sigma_cm) (add_proj proj_inv trans_sp_with_cm))
let _ = dump "species_annotated_frag2_proj_annotated_cm.dot" ["frag",2;"flow",1]
  (add_flow_cm (lift_site sigma_cm) (add_proj proj_inv trans_sp_with_flow_with_cm))


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
      [shc_Y7,[Direction n],[Free_site [Direction n]];
       shc_pi,[Direction se],[]];
      egfr,-3.,12.,[],
      [egfr_r,[Direction s],[Bound_site [Direction s]];
      egfr_Y48,[Direction (of_degree (to_degree nw+.5.));Scale 1.01],[Internal_state (egfr_Y48_p,[Direction (of_degree (to_degree nw+.20.))])]]]

let lhs_domain = add_link_list [rule_egfr_Y48,rule_shc_pi] lhs_domain 

let sigmal,_,_,rule = 
  build_rule 
    lhs_domain 
    (fun remanent -> 
      ([],[],[]),snd (add_internal_state rule_shc_Y7 shc_Y7_u [Direction ne] remanent))
    (fun remanent -> 
      ([],[],[]),snd (add_internal_state rule_shc_Y7 shc_Y7_p [Direction ne] remanent))
    [Direction s]

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

let proj_list sigmar sigmar = 
  List.map (fun (x,y) -> lift_agent sigmar x,lift_agent sigmal y) 

let proj_list_cm sigmar sigmal = 
  proj_list sigmar sigmal 
    [
      rule_shc,cm_shc;
      rule_egfr,cm_egfr
    ]
    
let proj_list_sp sigmar sigmal = 
  proj_list sigmar sigmal 
    [
      rule_shc,sp_shc2;
      rule_egfr,sp_egfr2
    ]
    
let add_flow sigma = 
  List.map (fun (x,y) -> lift_site sigma x,lift_site sigma y)

let add_flow_cm sigmal  = 
  add_flow sigmal 
    [ 
      cm_egfr_r,cm_egfr_Y48;
      cm_egfr_Y48,cm_shc_pi;
      cm_shc_pi,cm_shc_Y7
    ]

let add_flow_species sigmal = 
  add_flow sigmal 
    [
      sp_egfr2_r,sp_egfr2_Y48;
      sp_egfr2_Y48,sp_shc2_pi;
      sp_shc2_pi,sp_shc2_Y7
    ]
       
let _ = proj_rule_flow_on_a_species ~file:"flow_rule_proj_sp_"  annotated_rule (vertical_swap species) [rule_shc,sp_shc2;rule_egfr,sp_egfr2]

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
       egfr_Y68,[Direction s;Tag ("Exaa",0);Tag ("Exab",0);Tag("Exac",0)],[Free_site ([Direction sw]);Internal_state (egfr_Y68_u,[Direction (se)])];
       egfr_Y48,[Direction w;Tag ("Exad",0);Tag ("Exac",0)],[Internal_state (egfr_Y48_p,[Direction (of_degree 225.)])]
      ];
      egf,3.5,13.8,[],
      [egf_r,[Direction s],[]];
      egfr,3.5,12.,[],
       [egfr_l,[Direction n],[];
	egfr_r,[Direction w;Scale 0.9],[];
	egfr_Y68,[Direction s;Tag ("Exaa",0);Tag ("Exad",0);Tag("Exac",0)],[Internal_state (egfr_Y68_p,[Direction se])];
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
      [shc_Y7,[Direction w],[Bound_site[Direction nw]];
       shc_pi,[Direction e],[]];
      egfr,-3.,12.,[],
      [egfr_r,[Direction e],[Bound_site [Direction se]];
      egfr_Y48,[Direction w],[]]]

let lhs_domain = add_link_list [cons_egfr_Y48,cons_shc_pi] lhs_domain 

let sigmal,sigmar,_,rule = 
  build_rule 
    lhs_domain 
    (fun remanent -> 
      ([],[],[]),snd (add_internal_state cons_shc_Y7 shc_Y7_u [Direction ne] remanent))
    (fun remanent -> 
      ([],[],[]),snd (add_internal_state cons_shc_Y7 shc_Y7_p [Direction ne] remanent))
    [Direction e]


let _ = dump "flow_cons_rule.dot" ["contact_map",0;"flow",0]  rule 

let cons_egfr_l,fragment = add_site cons_egfr egfr_l [Direction s] lhs_domain 
let _,fragment = add_bound cons_egfr_l [Direction se] fragment 

let sigmarule,sigmafragment,cons =   disjoint_union rule (move_remanent_bellow 0. fragment lhs_domain)

let sigmalrule = compose_lift sigmarule sigmal 
let lift = 
  List.map
    (fun x -> lift_agent sigmalrule x,lift_agent sigmafragment x)
let pairing = lift [cons_egfr;cons_shc]
let cons1 = add_match pairing cons 

let cons2 = add_proj pairing cons 

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
      [shc_Y7,[Direction w],[Bound_site[Direction nw]];
       shc_pi,[Direction e],[]];
      egfr,-3.,12.,[],
      [egfr_l,[Direction s],[Bound_site  [Direction se]];
      egfr_Y48,[Direction w],[]]]

let _,fragment = add_bound frag_egfr_l [Direction se] fragment 

let sigmarule,sigmafragment,prod =   disjoint_union rule (move_remanent_bellow 0. (translate_graph {abscisse = 6.;ordinate =  0.} fragment) lhs_domain)
let sigmaruler = compose_lift sigmarule sigmar
let sigmarulel = compose_lift sigmarule sigmal
let lift = 
  List.map (fun (x,y) -> lift_agent sigmaruler x,lift_agent sigmafragment y)
let prod1 = add_match (lift [cons_egfr,frag_egfr;cons_shc,frag_shc]) prod
let st,prod2 = add_site (lift_agent sigmaruler cons_egfr) egfr_l [Direction s;Color "red"] prod1 
let _,prod2 = add_bound st [Color "red"] prod2 
let st,prod2 = add_site (lift_agent sigmarulel cons_egfr) egfr_l [Direction s;Color "red"] prod2
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
       egfr_Y68,[Direction s;Tag ("Exaa",0);Tag ("Exab",0);Tag("Exac",0)],[Free_site ([Direction sw]);Internal_state (egfr_Y68_u,[Direction (se)])];
       egfr_Y48,[Direction w;Tag ("Exad",0);Tag ("Exac",0)],[Internal_state (egfr_Y48_p,[Direction (of_degree 225.)])]
      ];
      egf,3.5,13.8,[],
      [egf_r,[Direction s],[]];
       egfr,3.5,12.,[],
       [egfr_l,[Direction n],[];
	egfr_r,[Direction w;Scale 0.9],[];
	egfr_Y68,[Direction e;Tag ("Exaa",0);Tag ("Exad",0);Tag("Exac",0)],[Internal_state (egfr_Y68_p,[Direction se])];
	egfr_Y48,[Direction s],[]
       ];
       shc,3.5,10.2,[],
      [shc_pi,[Direction n],[];
       shc_Y7,[Direction s],[Internal_state (shc_Y7_u,[Direction se]);Free_site [Direction s]]]]
      


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
(*let _ = dump "flow_species_sp_annotated.dot" ["species",0]  (snd (add_free sp_egfr2_r [] short_species))*)

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
      [shc_Y7,[Direction w],[Free_site[Direction nw]];
       shc_pi,[Direction e],[]];
      egfr,-3.,12.,[],
      [egfr_r,[Direction e],[Bound_site [Direction se]];
      egfr_Y48,[Direction w],[]]]

let lhs_domain = add_link_list [cons_egfr_Y48,cons_shc_pi] lhs_domain 




let _,_,_,rule = 
  build_rule 
    lhs_domain 
    (fun remanent -> 
      ([],[],[]),snd (add_internal_state cons_shc_Y7 shc_Y7_u [Direction sw] remanent))
    (fun remanent -> 
      ([],[],[]),snd (add_internal_state cons_shc_Y7 shc_Y7_p [Direction sw] remanent))
    [Direction s]


let _ = dump "flow_cons_rule.dot" ["contact_map",0;"flow",0]  rule 
