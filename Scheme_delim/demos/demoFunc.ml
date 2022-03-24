open Scheme_delim_lib.Interpreter

let demos =
  {| (define (factorial n)
  (if (<= n 0)
      1
      (* n (factorial (- n 1)))))

(display (factorial 0))
(newline)
(display (factorial 1))
(newline)
(display (factorial 2))
(newline)
(display (factorial 4))
(newline)
(display (factorial 5))
(newline)

(define (fib-rec n)
  (if (< n 2)
      n
      (+ (fib-rec (- n 1))
         (fib-rec (- n 2)))))
     
(display (fib-rec 2))
(newline)
(display (fib-rec 3))
(newline)
(display (fib-rec 4))
(newline)
(display (fib-rec 5))
(newline)

(define odd-positive?
  (lambda (x)
    (cond
      ((not (integer? x)) "A nonnegative number is required")
      ((= x 0) #f)
      ((< x 0) #f)
      (else (even? (- x 1)))))) 

(display (odd-positive? "string"))
(newline)
(display (odd-positive? -7))
(newline)
(display (odd-positive? 10))
(newline)
(display (odd-positive? 555))
(newline)

(display ((lambda (x) 
   (list x (list (quote quote) x))) 
  (quote 
     (lambda (x) 
       (list x (list (quote quote) x))))))

    |}
;;

let () =
  match run_program demos with
  | Ok _ -> ()
  | Error _ -> Format.printf "Test failed"
;;
