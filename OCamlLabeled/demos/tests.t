  $ ./tests.exe <<-EOF
  >  let fib n k = if n < 2 then n else fib (n-1) (fun l -> fib (n-2) (fun r -> k (l+r)));;
  val fib = <fun>
  =====================================
  $ ./tests.exe <<-EOF
  >  let rec fib ~n ~k = if n < 2 then k n else fib ~n:(n-1) ~k:(fun l -> fib ~n:(n-2) ~k:(fun r -> k (l+r)));;
  >  let ans = fib ~k:(fun x -> x) ~n:6;;
  val fib = <fun>
  val ans = 8
  =====================================
  $ ./tests.exe <<-EOF
  > let rec fold_left ~f init = function
  >  | [] -> init
  >  | hd :: tl -> fold_left ~f:f (f init hd) tl;;
  > let k = fold_left 0 [1;2;3] ~f:(fun acc x ->  acc + x * x);;
  val fold_left = <fun>
  val k = 14
  =====================================
  $ ./tests.exe <<-EOF
  > let f ?(x = 5) ~y ?(z = 3/3) t = t + x + y;;
  > let k = f ~y:5;;
  > let b = k 10;;
  val f = <fun>
  val k = <fun>
  val b = 20
  =====================================
  $ ./tests.exe <<-EOF
  >  let rec fix f x = f (fix f) x
  >  ;;
  >  let f self n = if n <= 1 then n else self (n - 1) * n
  >  ;;
  >  let fact10 = fix f 10
  val fix = <fun>
  val f = <fun>
  val fact10 = 3628800
  =====================================
  $ ./tests.exe <<-EOF
  >  let f = function | Some x :: xs -> 28 | [None] -> 42 | _ -> 1337;;
  >  let a = f [None];;
  val f = <fun>
  val a = 42
  =====================================
  $ ./tests.exe <<-EOF
  >  let x = ref 0;;
  >  let y = let _ = x := 1 in
  >  x;;
  val x = 1
  val y = 1
  =====================================
  $ ./tests.exe <<-EOF
  > let f =
  >   let n = ref 0 in
  >   (fun m -> let _ = n := 1 + !n in !n);;
  > let x1 = f 0;;
  > let x2 = f 0;;
  > let x3 = f 0;;
  > let x4 = f 0;;
  > let x5 = f 0;;
  val f = <fun>
  val x1 = 1
  val x2 = 2
  val x3 = 3
  val x4 = 4
  val x5 = 5
  =====================================
# Right behaviour of this test requires thinking about types
  $ ./tests.exe <<-EOF
  > let f ?(x=5) = x;;
  val f = <fun>
  =====================================
