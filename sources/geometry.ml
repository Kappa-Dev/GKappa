(**
 * geometry.ml 
 * GKappa
 * Jérôme Feret, projet Antique, INRIA Paris-Rocquencourt
 * 
 * Creation:                      <2015-04-05 feret>
 * Last modification: Time-stamp: <2015-05-27 15:17:52 feret>
 * * 
 *  
 * Copyright 2015 Institut National de Recherche en Informatique  * et en Automatique.  All rights reserved.  
 * This file is distributed under the terms of the 
 * GNU Library General Public License *)


(* GKappa is a OCAML library to help making slides in Kappa *)
(* More applications are coming soon (hopefully) *)

let pi = 3.14159265359	

type point = 
  { 
    abscisse: float ; 
    ordinate: float 
  }


let origin = 
  {
    abscisse = 0.;
    ordinate = 0.
  }

type angle = 
  { 
    radius: float ;
    degree: float 
  }

type angle_decl = Radius of float | Degree of float 
let angle_of_decl x = 
  match x 
  with 
    Radius x -> { radius=x;degree=(x*.360.)/.2.*.pi}
  | Degree x -> { radius=(x*.2.*.pi)/.360. ; degree=x}

let to_degree x = x.degree 
let of_degree x = angle_of_decl (Degree x)
let to_radius x = x.radius 
      
let n =  of_degree 0.
let ne = of_degree 45.
let e =  of_degree 90.
let se = of_degree 135.
let s =  of_degree 180.
let sw = of_degree 225.
let w =  of_degree 270.
let nw = of_degree 315.

let bissec angle1 angle2 = 
  angle_of_decl (Degree ((angle1.degree +. angle2.degree)/.2.))

let clockwise angle1 angle2 = 
  angle_of_decl (Degree ((angle1.degree +. angle2)))

let anticlockwise angle1 angle2 = 
  angle_of_decl (Degree ((angle1.degree -. angle2)))

let rec mod_angle x = 
  if x<0. then mod_angle (x+.360.)
  else if x>360. then mod_angle (x-.360.)
  else x 

let sample_angle x = 
  let angle = mod_angle (to_degree x) in 
  let rec scan x rep = 
    if angle < x then of_degree rep
    else scan (x+.45.) (rep+.45.)
  in scan 23. 0. 

 let point_on_ellipse_ext center width height  direction scale delta = 
      let angle = direction.radius in 
      {
	abscisse = center.abscisse +. (sin angle) *. (width *. 0.5 *. scale +. delta);
	ordinate = center.ordinate +. (cos angle) *. (height *. 0.5 *. scale +. delta)
      }
	
 let point_on_ellipse center width height direction scale = 
      point_on_ellipse_ext center width height direction scale 0.

  let point_on_rectangle_ext center width height  direction scale delta = 
      let angle = mod_angle direction.radius in 
      let angle_rectangle = atan (width/.height) in 
      if angle <= angle_rectangle 
      then {
	ordinate = center.ordinate +. (height *. 0.5 *. scale +. delta) ; 
	abscisse = center.abscisse +. (height *. 0.5 *. (tan angle) *. scale +.delta) }
      else if angle <= pi -. angle_rectangle 
      then {
	abscisse = center.abscisse +. (width *. 0.5 *.scale +. delta) ; 
	ordinate = center.ordinate +. (width *. 0.5 *. (tan angle) *. scale +. delta) }
      else if angle < pi +. angle_rectangle 
      then  {
	ordinate = center.ordinate -. (height *. 0.5 *. scale +. delta) ; 
	abscisse = center.abscisse -. (height *. 0.5 *. (tan angle) *. scale +. delta)}
      else {
	abscisse = center.abscisse -. (width *. 0.5 *.scale +. delta) ; 
        ordinate = center.ordinate -. (width *. 0.5 *. scale *. (tan angle) +. delta) }
	
 let point_on_rectangle center width height direction scale = 
      point_on_rectangle_ext center width height direction scale 0. 

let point_on_hexagone_ext center width height  direction scale delta = 
      let angle = mod_angle direction.radius in 
      if width > height 
      then 
	begin 
	  if angle <= pi/.4. 
	    ||
	      (angle >= 3./.4.*.pi && angle <= 5./.4.*.pi)
	    || angle >= 7./.4.*.pi 
	  then 
	    point_on_rectangle_ext center height height direction scale delta
	  else
	     point_on_ellipse_ext center width height direction scale delta
	end
      else 
	begin 
	   if (angle >= pi/.4. && angle <= 3./.4.*.pi)
	    ||
	      (angle >= 5./.4.*.pi && angle <= 7./.4.*.pi)
	  then 
	    point_on_rectangle_ext center width width direction scale delta
	  else 
	     point_on_ellipse_ext center width height direction scale delta
	end 
	
	
 let point_on_hexagone center width height direction scale = 
      point_on_hexagone_ext center width height direction scale 0.


 let translate x y = 
   {abscisse = x.abscisse +. y.abscisse;
    ordinate = x.ordinate +. y.ordinate
   }
     

let n_modulo list n = 
  let rec aux n l = 
    match l 
    with [] -> aux n list 
    | t::q -> 
      if n=1 then t else aux (n-1) q 
  in 
  if n<1 || list = [] then 
    (let _ = Printf.fprintf stderr "403\n" in 
     raise Exit)
  else aux n list 
    
let angle_of_index i = 
  let all_angles = [ne;se;sw;nw] in 
  let update l = 
    match l 
    with [] | [_]-> l,l
    | init::_ -> 
      let rec aux list (res,res2) = 
	match list 
	with a::b::c -> 
	      let x = bissec a b in 
	      aux (b::c) (x::a::res,x::res2)
	| [b] -> 
	  let x = bissec init b in 
	  List.rev (b::x::res),List.rev (x::res2 )
	| [] -> 
	  let _ = Printf.fprintf stderr "Warning: no angle are provided in the config filte\n" in 
	  aux [n] ([],[])
      in 
      aux l ([],[])
  in 
  let current_angles = all_angles in 
  let rec aux k all current = 
    match current 
    with []  -> 
      let all,current = update all in 
      aux k all current
    | t::q -> 
      if k=1 then t
      else aux (k-1) all q 
  in 
  aux i all_angles current_angles

let abs x = 
  if compare x 0. < 0  then -.x else x 

let compute_padding (xm,xM,ym,yM) (xm',xM',ym',yM') angle distance = 
  let distancex = abs (sin (to_radius angle)*. distance) in 
  let distancey = abs (cos (to_radius angle)*.distance) in 
  let cx,deltax = 
    if angle = n || angle = s 
    then 
      (xm+.xM)/.2.,
      (xm+.xM)/.2.-.(xm'+.xM')/.2.
    else if angle = ne || angle = e || angle = se 
    then 
      xM+.distancex/.2.,
      xM+.distancex-.xm'
    else if angle = sw || angle = w || angle = se 
    then 
      xm-.distancex/.2.,
      xm-.distancex+.xM' 
    else 
      0.,0.
  in 
  let cy,deltay = 
    if angle = e || angle = w
    then 
      (ym+.yM)/.2.,
      (ym+.yM)/.2.-.(ym'+.yM')/.2.
    else if angle = ne || angle = n || angle = nw 
    then 
      yM+.distancey/.2.,
      yM+.distancey-.ym'
    else if angle = sw || angle = s || angle = se 
    then 
      ym-.distancey/.2.,
      ym-.distancey-.yM' 
    else 
      0.,0.
  in 
  cx,cy,deltax,deltay
