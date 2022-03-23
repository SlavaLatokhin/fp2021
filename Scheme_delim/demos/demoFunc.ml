open Scheme_delim_lib.Interpreter

let demos =
  {| (define (factorial n)
  (if (<= n 0)
      1
      (* n (factorial (- n 1)))))

(display (factorial 0))
(display (factorial 1))
(display (factorial 2))
(display (factorial 4))
(display (factorial 5))

(define (fib-rec n)
  (if (< n 2)
      n
      (+ (fib-rec (- n 1))
         (fib-rec (- n 2)))))
     
(display (fib-rec 2))
(display (fib-rec 3))
(display (fib-rec 4))
(display (fib-rec 5))

(define odd-positive?
  (lambda (x)
    (cond
      ((not (integer? x)) "A nonnegative number is required")
      ((= x 0) #f)
      ((< x 0) #f)
      (else (even? (- x 1)))))) 

(display (odd-positive? "string"))
(display (odd-positive? -7))
(display (odd-positive? 10))
(display (odd-positive? 555))
    |}
;;

let () =
  match run_program demos with
  | Ok _ -> ()
  | Error _ -> Format.printf "Test failed"
;;
