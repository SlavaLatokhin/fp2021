Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

  $ ./demo_launcher.exe <<-EOF
  > ./rbs/anonymous.rb
  タイムマシン

  $ ruby ./rbs/anonymous.rb
  タイムマシン

  $ ./demo_launcher.exe <<-EOF
  > ./rbs/closure.rb
  2

  $ ruby ./rbs/closure.rb
  2

  $ ./demo_launcher.exe <<-EOF
  > ./rbs/method_missing.rb
  42
  no such method!

  $ ruby ./rbs/method_missing.rb
  42
  no such method!

  $ ./demo_launcher.exe <<-EOF
  > ./rbs/multiple_assignment.rb
  1
  2
  13
  :)
  20

  $ ruby ./rbs/multiple_assignment.rb
  1
  2
  13
  :)
  20

  $ ./demo_launcher.exe <<-EOF
  > ./rbs/object.rb
  13
  1998

  $ ruby ./rbs/object.rb
  13
  1998

  $ ./demo_launcher.exe <<-EOF
  > ./rbs/recursion.rb
  55
  3628800

  $ ruby ./rbs/recursion.rb
  55
  3628800
