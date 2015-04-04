(**
 * data_structures.ml 
 * GKappa
 * Jérôme Feret, projet Antique, INRIA Paris-Rocquencourt
 * 
 * Creation:                      <2015-03-28 feret>
 * Last modification: Time-stamp: <2015-04-04 17:08:29 feret> 
 * * 
 *  
 * Copyright 2015 Institut National de Recherche en Informatique  * et en Automatique.  All rights reserved.  
 * This file is distributed under the terms of the 
 * GNU Library General Public License *)


(* GKappa is a OCAML library to help making slides in Kappa *)
(* More applications are coming soon (hopefully) *)

(* Maps with parallel exploration of two maps. 
   Taken from KaSa *)

module type OrderedType = sig 
  type t 
  val compare : t -> t -> int 
end
  
module type Map = sig
  type key
  type 'a t 
  
  val empty: 'a t
  val is_empty: 'a t -> bool
  val add:  key -> 'a -> 'a t -> 'a t
  val find_option :  key -> 'a t -> 'a option 
  val remove:  key -> 'a t -> 'a t
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val map: ('a -> 'b) -> 'a t -> 'b t
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b            val map2:  ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t 
  val fold2: (key -> 'a  -> 'a  -> 'b  ->  'b) -> (key -> 'a   -> 'b  -> 'b) -> (key -> 'a  -> 'b  -> 'b) ->  'a t -> 'a t -> 'b -> 'b 
end

module Make(Ord:OrderedType) =  
  (struct
      
    type key = Ord.t
    type 'data t =  
      Empty 
    | Node of 'data t * key * 'data * 'data t * int
	
    let empty = Empty 
      
    let is_empty x = x==empty
      
    let height map = 
      match map with 
      | Empty -> 0
      | Node(_,_,_,_,h) -> h
        
    let create left key0 data right  = 
      Node (left,key0,data,right, 1 + max (height left) (height right))
	
    let rec find_option  key map = 
      match map with  
      | Empty -> None             
      | Node (left,key_map,data,right,_) -> 
        let cmp = compare key key_map in 
        if cmp = 0 then Some data
        else if cmp>0 then find_option  key right  
        else find_option  key left
          
    let balance  left key data right = 
      let height_left = height left in
      let height_right = height right in 
      begin
	if height_left <= height_right + 2 
	then
          begin
            if height_right <= height_left + 2 
            then create left key data right
            else 
              begin
		match right with 
		| Empty -> failwith "Inconsistent data_structure in Data_structures.balance"
		| Node (left0,key0,data0,right0,_) -> 
		  begin
		    if height left0 <= height right0 
		    then 
                      create (create left key data left0) key0 data0 right0
		    else
                      begin
			match left0 with 
			| Empty -> failwith "Inconsistent data_structure in Data_structures.balance"
			| Node (left1,key1,data1,right1,_) -> 
			    
			  create
                            (create left key data left1)
                            key1
                            data1 
                            (create right1 key0 data0 right0)
		      end  
		  end
              end
          end
	else
          begin
            match left with 
            | Empty -> failwith "Inconsistent data_structure in Data_structures.balance"
            | Node (left0,key0,data0,right0,_) -> 
              begin
                if height right0 <= height left0 
                then 
                  create left0 key0 data0 (create right0 key data right)
                else 
                  begin
                    match right0 with 
                    | Empty -> failwith "Inconsistent data_structure in Data_structures.balance"
                    | Node (left1,key1,data1,right1,_) -> 
                      
                      create
                        (create left0 key0 data0 left1)
                        key1
                        data1 
                        (create right1 key data right)
                  end
              end
          end  
      end
	
    let rec add  key data map = 
      match map with 
      | Empty -> Node (empty,key,data,empty,1)
      | Node (left,key_map,data_map,right,height)->
        let cmp = compare key key_map in 
        if cmp = 0 then 
          Node(left,key_map,data,right,height)
        else if cmp < 0 then 
          balance (add key data left) key_map data_map right 
        else 
          balance left key_map data_map (add key data right)
            
    let rec min_binding map key data = 
      match map with 
	Empty -> (key,data)
      | Node (left2,key2,data2,_,_) -> 
        min_binding left2 key2 data2 
	  
    let rec remove_min_binding  map key data map' = 
      match map with 
      | Empty -> map'
      | Node (left2,key2,data2,right2,_) ->
        balance (remove_min_binding left2 key2 data right2)  key2 data2 right2
          
    let merge  map1 map2 = 
      match map1 with 
      | Empty -> map2 
      | Node(left1,key1,data1,right1,_) ->
        begin
          match map2 with 
          | Empty -> map1 
          | Node(left2,key2,data2,right2,_) -> 
            let (key3,data3) = min_binding left2 key2 data2 in 
	    balance map1 key3 data3 (remove_min_binding  left2 key2 data2 right2)
        end  
          
    let rec remove  key map = 
      match map with 
      | Empty -> empty
      | Node (left,key,data,right,_) ->
        let cmp = compare key key in 
        if cmp = 0 
        then merge  left right
        else if cmp < 0 
        then 
          balance (remove key left)  key data right
        else 
          balance left key data (remove key right) 
	    
    let rec map f m = 
      match m with
      | Empty -> empty
      | Node(left,key,data,right,height) ->
        Node(map f left,key,f data,map f right,height)
          
    let rec iter f map = 
      match map with 
      | Empty -> ()
      | Node(left,key,data,right,_) -> 
        let _ = iter f left in 
        let _ = f key data in 
        iter f right
	  
    let rec fold f map value =
      match map with 
      | Empty -> value 
      | Node(left,key,data,right,_) ->
	fold f right (f key data (fold f left value))
          
    let rec cut_opt value map = 
      match map with 
      | Empty -> None 
      | Node (left1,key1,data1,right1,height1) -> 
	let cmp = Ord.compare value key1 in 
	if cmp = 0 then
          Some (left1,data1,right1)
	else if cmp < 0 then 
          match cut_opt value left1 with  
          | None -> None 
          |Some (left2,data2,right2) -> 
            Some (left2,data2,Node(right2,key1,data1,right1,height1))
	else 
          match cut_opt value right1 with 
          | None -> None
          | Some (left2,data2,right2) -> 
            Some (Node(left1,key1,data1,left2,height1),data2,right2)
              
    let rec join  left key value right =
      match balance  left key value right with 
      | Empty -> failwith "inconsistent data structure in Data_structures.join"
      | (Node (left2,key2,data2,right2,_) as map2) -> 
        let h = height left2 - height right2 in 
        if h > 2 || h< -2 
        then join left2 key2 data2 right2 
        else map2 
	  
    let rec split  value map = 
      match map with 
      | Empty -> (empty,None,empty) 
      | Node (left1,key1,data1,right1,_) -> 
        let cmp = Ord.compare value key1 in 
        if cmp = 0 then
          (left1,Some data1,right1)
        else if cmp < 0 then 
          let (left2,data2,right2) = split  value left1 in 
          let right2' = join right2 key1 data1 right1 in     
          (left2,data2,right2')
        else 
          let (left2,data2,right2) = split  value right1 in
          let left2' = join left1 key1 data1 left2 in  
          (left2',data2,right2)
            
    let rec update  map1 map2 =
      if map1==map2 then map2 
      else  
	match map1 with 
        | Empty -> map2 
        | Node(left1,key1,data1,right1,_) -> 
          let (left2,data2,right2) = split  key1 map2 in 
          let left' = update left1 left2 in 
          let right' = update right1 right2 in 
          join left' key1 
            (match data2 with None -> data1 | Some d2 -> d2)
            right' 
            
    let rec map2  f m1 m2 =
      match m1 with 
      | Empty -> m2 
      | Node(left1,key1,data1,right1,_) -> 
        let left2,data2,right2 = split key1 m2 in 
        let left' = map2 f left1 left2 in 
        let right' = map2 f right1 right2 in 
        join left' key1 
          (match data2 with None -> data1 | Some d2 -> f data1 d2)
          right'       
        
    (*fold2 on map1 and map2 in paraller, do not scan association with the same address *)
    let rec fold2  f g h map1 map2 res =
      if map1==map2 then res 
      else 
	match map1,map2 with 
      | Empty,Empty -> res 
      | Empty , _ -> fold h map2 res 
      | _ , Empty -> fold g map1 res 
      | Node(left1,key1,data1,right1,_),_ -> 
        let left2,data2,right2 = split  key1 map2 in 
        begin 
          match data2 with 
          | None -> 
	    let res' = fold2 f g h left1 left2 res in 
            let res'' = g key1 data1 res' in
            fold2 f g h right1 right2 res''
          | Some data2 -> 
            let res' = fold2 f g h left1 left2 res in 
	    let res'' = 
	      if data1 ==data2 
	      then res'
	      else f key1 data1 data2 res' 
	    in
            fold2 f g h right1 right2 res''
        end
          
                     
end:Map with type key = Ord.t)
      
    
  
  
