(* ============================= *)
(* Lab 6: Introduction to Fold-left and Fold-right *)
(* ============================= *)

(* ----------------------------- *)
(* myFoldl: Left fold *)
let rec myFoldl op acc lst =
  match lst with
  | [] -> acc
  | h :: t -> myFoldl op (op acc h) t

(* ----------------------------- *)
(* myFoldr: Right fold *)
let rec myFoldr op lst acc =
  match lst with
  | [] -> acc
  | h :: t -> op h (myFoldr op t acc)

(* ----------------------------- *)
(* myReverse: Reverse a list using optional accumulator *)
let rec myReverse ?(acc=[]) lst =
  match lst with
  | [] -> acc
  | h :: t -> myReverse ~acc:(h :: acc) t

(* ----------------------------- *)
(* myReverseFold: Reverse using fold-left *)
let myReverseFold lst =
  myFoldl (fun acc x -> x :: acc) [] lst

(* ----------------------------- *)
(* myMap: Apply function to every element, recursive, optional accumulator *)
let rec myMap ?(acc=[]) op lst =
  match lst with
  | [] -> myReverse ~acc:acc []
  | h :: t -> myMap ~acc:(op h :: acc) op t

(* ----------------------------- *)
(* myMapFold: Apply function using fold-right *)
let myMapFold op lst =
  myFoldr (fun x acc -> op x :: acc) lst []

(* ----------------------------- *)
(* myFilter: Filter list by guard function, optional accumulator *)
let rec myFilter ?(acc=[]) guard lst =
  match lst with
  | [] -> myReverse ~acc:acc []
  | h :: t ->
      if guard h then myFilter ~acc:(h :: acc) guard t
      else myFilter ~acc:acc guard t

(* ----------------------------- *)
(* myFilterFold: Filter list using fold-right *)
let myFilterFold guard lst =
  myFoldr (fun x acc -> if guard x then x :: acc else acc) lst []