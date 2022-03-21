open Scheme_call_lib.Interpreter

(* 
  Implementations of factorial with simple recursion.
  Two implementation of factorial with Y-combinator:
  head-recursive and tail-recursive.
  Mutual recursion 
*)

let recursion =
  {|
(define factorial-aps
  (lambda (n answer)
    (cond 
    ((zero? n) answer)
    (else (factorial-aps (- n 1) (* n answer)))
)))

(define fac2
    (lambda (n)
      (factorial-aps n 1)))

(display (fac2 5))
(newline)



(define Y
  (lambda (f)
    ((lambda (g) (g g))
     (lambda (g)
       (f  (lambda a (apply (g g) a)))))))

(define fac3
  (Y (lambda (r)
       (lambda (x)
         (if (< x 2)
             1
             (* x (r (- x 1))))))))

(define fac4
  (lambda (x)
    ((Y (lambda (r)
          (lambda (x acc)
            (if (< x 2)
                acc
                (r (- x 1) (* x acc))))))
     x 1)))

(display (fac3 6))
(newline)
(display (fac4 7))
(newline)



(define odd-positive?
  (lambda (x)
    (cond
      ((not (integer? x)) "Error. x must be a nonnegative number")
      ((= x 0) #f)
      ((< x 0) "Error. x must be a nonnegative number")
      (else (even? (- x 1))))))

(define even-positive?
  (lambda (x)
    (cond
      ((not (integer? x)) "Error. x must be a nonnegative number")
      ((= x 0) #t)
      ((< x 0) "Error. x must be a nonnegative number")
      (else (odd? (- x 1))))))

(display (even-positive? 4))
(newline)
(display (even-positive? 5))
(newline)
(display (odd-positive? 6))
(newline)
(display (odd-positive? 7))
(newline)
|}
;;

let () =
  match parse_and_run_prog recursion with
  | Ok _ -> ()
  | Error err -> Printf.printf "%s" err
;;
