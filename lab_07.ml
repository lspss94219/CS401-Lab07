(*
    Lab 7: Introduction to tail recursion
    cc: Akshar Patel (akshar20@uab.edu), Michael Gathara (mikegtr@uab.edu)

    This assignment will help you familiarize yourself with the idea of tail recursion
    To do so, we will be implementing fibonacci in a non-tail and tail recursive manner.

    After implementing these functions, please make a report:
        (1) explaining the differences in implementation between your non-tail and tail
        recursive implementations
        (2) discussing why tail recursion is a preferred way to implement functions like
        factorial (hint: we talked about it in lab lecture)

    Submission:
    - On Canvas: Required: This file, Report (pdf)
    - On Github: Required: This file
                 Optional: Report
*)

(*
  Fibonacci: A mathematical function that takes in an integer
  n and returns the nth number in the fibonacci sequence 
  where the nth number is the sum of the numbers at n-1 and n-2

  example: fibonacci 4 will output 3 since
  fib(4) = fib(3) + fib(2)
  = fib(2) + fib(1) + fib(1) + fib(0)
  = fib(1) + fib(0) + fib(1) + fib(1) + fib(0)
  = 1 + 0 + 1 + 1 + 0
  = 3

  fib must be implemented in a non-tail recursive manner
  fibTail must be implemented in a tail-recursive manner

  Input:
      - n: the integer you must calculate fib(n) of

*)

(* Non-tail recursive Fibonacci *)
let rec fib n =
  if n < 0 then
    raise (Failure "Negative input not allowed")
  else if n = 0 then 0
  else if n = 1 then 1
  else fib (n-1) + fib (n-2)
;;

(* Tail-recursive Fibonacci *)
let rec fibTail ?(a=0) ?(b=1) n =
  if n < 0 then
    raise (Failure "Negative input not allowed")
  else if n = 0 then a
  else if n = 1 then b
  else fibTail ~a:b ~b:(a+b) (n-1)
;;

(* Test examples *)
let () =
  Printf.printf "Non-tail fib(10) = %d\n" (fib 10);
  Printf.printf "Tail-recursive fibTail(10) = %d\n" (fibTail 10);
;;