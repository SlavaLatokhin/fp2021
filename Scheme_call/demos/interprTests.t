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
