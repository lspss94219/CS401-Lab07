(*
    Lab 8: The Overlapping Line Coverage Problem
    cc: Akshar Patel (akshar20@uab.edu), Michael Gathara (mikegtr@uab.edu)

    This assignment will cover implementing the overlapping line coverage problem. This
    problem entails recieving a list of lines that may overlap in 1-dimensional space
    and calculating the total distance the lines cover in a time-efficient manner.
    The solution to this problem requires representing the 1-dimensional space as a
    binary tree.

    Lines in this problem are represented as float * float tuples where the first value
    is the beginning of the line while the second is the end of the line. Note that lines
    where the values are the same or the first value is greater than the second would be
    considered lines of length zero.

    You must implement insertLine, sumLineTree, and lineCoverage. We supplied a shuffle
    function because the inputs must be shuffled to ensure a good time complexity for
    this algorithm. We also supplied the data structure you will use to represent your
    binary trees.

    After implementing these functions, please make a report:
        (1) Please explain the algorithm briefly in your own words
        (2) Give the runtime of the algorithm and a brief justification
        (3) Include screenshots of you running our tests cases and getting the desired
        output in utop

    Submission:
    - On Canvas: Required: This file, Report (pdf)
    - On Github: Required: This file
                 Optional: Report
*)

type 'a linetree =
  | Empty
  | Covered
  | LineTree of {
    p: float;
    left: 'a linetree;
    right: 'a linetree;
  }

let shuffle lst =
  let nd = List.map (fun c -> (Random.bits (), c)) lst in
  let sond = List.sort compare nd in
  List.map snd sond

(*
  insertLine: This function will take a line and insert it
  into our tree representation of 1-dimensional space,
  returning that updated tree

  Input:
      - tree: a binary tree using the linetree datastructure
      - line: a float * float tuple encoding one line
  Output:
      - An updated linetree with the line added

  Strategy:
    Each LineTree node holds a split point p created from a line
    endpoint.  left covers (-inf, p) and right covers [p, +inf).
    When inserting [a,b]:
      - If tree is Empty, create a node at p=a with right child
        LineTree{p=b; left=Covered; right=Empty} to mark exactly [a,b).
      - If tree is Covered, the region is already fully covered.
      - Otherwise recurse left if b <= p, right if a >= p, or both
        if the line straddles p.  Collapse to Covered when both
        children become Covered.
  *)
let rec insertLine tree line =
  let (a, b) = line in
  if a >= b then tree    (* zero-length or inverted line → no coverage *)
  else match tree with
  | Covered -> Covered   (* already fully covered, nothing to do *)
  | Empty ->
    (* Create split at a; the interval [a,b) is marked covered via a
       second split at b inside the right subtree. *)
    LineTree {
      p = a;
      left = Empty;
      right = LineTree { p = b; left = Covered; right = Empty };
    }
  | LineTree { p; left; right } ->
    if b <= p then
      (* Entire line lies to the left of p *)
      let new_left = insertLine left (a, b) in
      (match new_left, right with
       | Covered, Covered -> Covered
       | _ -> LineTree { p; left = new_left; right })
    else if a >= p then
      (* Entire line lies to the right of p (or starts at p) *)
      let new_right = insertLine right (a, b) in
      (match left, new_right with
       | Covered, Covered -> Covered
       | _ -> LineTree { p; left; right = new_right })
    else
      (* Line straddles p: recurse into both sides *)
      let new_left  = insertLine left  (a, p) in
      let new_right = insertLine right (p, b) in
      (match new_left, new_right with
       | Covered, Covered -> Covered
       | _ -> LineTree { p; left = new_left; right = new_right })

(*
  sumLineTree: A function that takes in a binary tree encoding 
  1-dimensional space and outputs the total length covered by
  the lines inserted into the tree

  Input:
      - tree: a binary tree using the linetree datastructure
  Output:
      - A float giving the length covered by the lines

  s and e are the real-number bounds of the current region
  (default -inf and +inf at the root).
  - Covered → the full region [s, e) is covered, contribute e - s.
  - Empty   → nothing covered, contribute 0.
  - LineTree{p,...} → split: left owns [s, p), right owns [p, e).
  *)
let rec sumLineTree ?(s=neg_infinity) ?(e=infinity) tree =
  match tree with
  | Empty   -> 0.0
  | Covered -> e -. s
  | LineTree { p; left; right } ->
    sumLineTree ~s ~e:p left +. sumLineTree ~s:p ~e right

(*
  lineCovereage: A function that takes the two previous
  functions to perform the overlapping line coverage problem.
  Do not forget to shuffle the order in which you insert your
  lines in the binary tree.

  Input:
      - lines: A list of lines encoded as float * float tuples
  Output:
      - A float giving the length covered by the lines
  *)
let lineCoverage lines =
  let shuffled = shuffle lines in
  let tree = List.fold_left insertLine Empty shuffled in
  sumLineTree tree



(*-----------------------------TEST CASES-----------------------------*)

(*ANSWER = 0*)
let testInput1 = []

(*ANSWER = 110*)
let testInput2 = [(1.,2.); (2.,3.); (4.,10.); (8.,12.); (50.,100.); (-100.,-50.)]

(*ANSWER = 0*)
let testInput3 = [(2., 1.); (5000., 5000.); (5., 5.); (100., 99.); (99., 0.); (infinity, neg_infinity)]

(*ANSWER = infinity*)
let testInput4 = [(0., 1.); (4., 5.); (neg_infinity, 0.); (102321., 4000000.)]

(*ANSWER = 121.876876180545253*)
let testInput5 = [(1.12312341, 2.12398719823); (2.1231234, 3.); (3.12312451, 100.12312412341); (98.1231231, 123.12312410054525)]

let () =
  Printf.printf "Test 1 (expected 0): %f\n"      (lineCoverage testInput1);
  Printf.printf "Test 2 (expected 110): %f\n"    (lineCoverage testInput2);
  Printf.printf "Test 3 (expected 0): %f\n"      (lineCoverage testInput3);
  Printf.printf "Test 4 (expected inf): %f\n"    (lineCoverage testInput4);
  Printf.printf "Test 5 (expected 121.88): %f\n" (lineCoverage testInput5)