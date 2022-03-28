Some implementations of factorial algoritms

  $ ./demoRecursion.exe
  120
  720
  5040
  #t
  #f
  #f
  #t

  $ ./demoParse.exe <<-EOF
  > (display (= 4
  > (+ 1 (call/cc
  > (lambda (ret)
  > (+ 2 (ret 3)))))))
  > (newline)
  > 
  > (define (f return)
  > (return 2) 3)
  > 
  > (display (f (lambda (x) x)))
  > (newline)
  > 
  > (display (call-with-current-continuation f))
  #t
  3
  2

  $ ./demoParse.exe <<-EOF
  > (display (+ 1 1 1 1 1 1))
  6
  $ ./demoParse.exe <<-EOF
  > (display (+ 1 (+ 2 (+ 3 (+ 4 5)))))
  15
  $ ./demoParse.exe <<-EOF
  > (display (* 1 2 3 4 5))
  120
  $ ./demoParse.exe <<-EOF
  > (display (>= 10 8 4 2 0 0))
  #t
  $ ./demoParse.exe <<-EOF
  > (display (car (list 1 2 3)))
  1
  $ ./demoParse.exe <<-EOF
  > (display (cdr (list 1 2 3)))
  (2 3)
  $ ./demoParse.exe <<-EOF
  > (display (car 1))
  Exception in car: invalid variable type
  $ ./demoParse.exe <<-EOF
  > (display (cdr (list 1) (list 2)))
  Exception in cdr: incorrect argument count

# Quotes
  $ ./demoParse.exe <<-EOF
  > (display '(1 42 13 (+ 10 20 (* 5 15))) )
  (1 42 13 (+ 10 20 (* 5 15)))

  $ ./demoParse.exe <<-EOF
  > (display '((lambda (x) (* x x)) 5) )
  ((lambda (x) (* x x)) 5)

  $ ./demoParse.exe <<-EOF 
  > (display '(+ 1 '(+ 3 4)) )
  (+ 1 '(+ 3 4))

  $ ./demoParse.exe <<-EOF 
  > (display '(+ 1 ,(+ 3 4)) )
  (+ 1 ,(+ 3 4))

  $ ./demoParse.exe <<-EOF 
  > (display '(+ 1 \`(+ 3 4)) )
  > (newline)
  > (display \`(+ 1 '(3 4)) ) 
  (+ 1 `(+ 3 4))
  (+ 1 '(3 4))
#Hangs ?
  $ ./demoParse.exe <<-EOF
  > (display \`(+ 1 \`(+ 3 4)) )
  (+ 1 `(+ 3 4))
#Hangs ?
  $ ./demoParse.exe <<-EOF
  > (display \`(+ 1 ,(+ 3 4)) )
  (+ 1 7)
  $ ./demoParse.exe <<-EOF
  > (display \`(+ 1 '(+ 3 4 ,(* 3 5))) )
  (+ 1 '(+ 3 4 15))
  $ ./demoParse.exe <<-EOF
  > (display (list 7 6 5 ,(+ 1 2 3)) )
  Exception: invalid syntax
# call/cc Quines
# HANGS?
  $ ./demoParse.exe <<-EOF
  > (display (call/cc (lambda (c) (c ((lambda (c) \`(call/cc (lambda (c) (c (,c ',c))))) '(lambda (c) \`(call/cc (lambda (c) (c (,c ',c))))))))))
  (call/cc (lambda (c) (c ((lambda (c) `(call/cc (lambda (c) (c (,c ',c))))) '(lambda (c) `(call/cc (lambda (c) (c (,c ',c)))))))))
  $ ./demoParse.exe <<-EOF
  > (display (call/cc (lambda (c)  (call/cc (lambda (cc)  (c ((lambda (c) \`(call/cc (lambda (c) (call/cc (lambda (cc) (c (,c ',c))))))) '(lambda (c) \`(call/cc (lambda (c) (call/cc (lambda (cc) (c (,c ',c))))))))))))) )
  (call/cc (lambda (c) (call/cc (lambda (cc) (c ((lambda (c) `(call/cc (lambda (c) (call/cc (lambda (cc) (c (,c ',c))))))) '(lambda (c) `(call/cc (lambda (c) (call/cc (lambda (cc) (c (,c ',c)))))))))))))

# if
  $ ./demoParse.exe <<-EOF
  > (display (if (> 2 1) 42 'false) )
  42

# fac
  $ ./demoParse.exe <<-EOF
  > (define (fac n) (if (<= n 1) 1 (* n (fac (- n 1)))))
  > (display (fac 5) )
  120
  $ ./demoParse.exe <<-EOF
  > (define fac (lambda (n) (if (< n 1) 1 (* n (fac (- n 1))))))
  > (display (fac 5) )
  120

# call/cc
  $ ./demoParse.exe <<-EOF
  > (display (+ 1 (call/cc (lambda (k) (+ 2 (k 3))))) )
  4
  $ ./demoParse.exe <<-EOF
  > (display (+ 1 (call/cc (lambda (l) (define a 1) (+ 10 (l a) 15)))) )
  2
  $ ./demoParse.exe <<-EOF
  > (display (+ 1 (call/cc (lambda (l) (* 10 (l (call/cc (lambda (l) (/ 15 (l 5))))))))) )
  6
  $ ./demoParse.exe <<-EOF
  > (display (+ 1 (call/cc (lambda (l) (* 10 (l (* 10 (call/cc (lambda (l) (/ 15 (l 5)))))))))) )
  51
  $ ./demoParse.exe <<-EOF
  > (display (+ 1 (call/cc (lambda (l) (* 10 (l (call/cc (lambda (k) (/ 15 (k 5))))))))) )
  6





  $ ./demoParse.exe <<-EOF
  > (define list-product
  > (lambda (s)
  > (let recur ((s s))
  > (if (null? s) 1
  > (* (car s) (recur (cdr s)))))))
  > 
  > (display (list-product '(1 2 3 4 5)))
  > (newline)
  > 
  > (define list-product2
  > (lambda (s)
  > (call/cc
  > (lambda (exit)
  > (let recur ((s s))
  > (if (null? s) 1
  > (if (= (car s) 0) (exit "NULL")
  > (* (car s) (recur (cdr s))))))))))
  > 
  > (display (list-product2 '(1 2 3 4 5)))
  > (newline)
  > 
  > (display (list-product2 '(1 2 3 4 0 5)))
  > (newline)
  120
  120
  NULL

