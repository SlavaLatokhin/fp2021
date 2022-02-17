open Csharp_lib.Parser

open
  Csharp_lib.Interpret_classes.Interpret_classes
    (Csharp_lib.Interpret_classes.Result)

open Csharp_lib.Interpreter.Interpreter (Csharp_lib.Interpret_classes.Result)

let test_interp class_list class_table =
  match interpret_classes class_list class_table with
  | Error m -> print_endline m ; Hashtbl.clear class_table
  | Ok load_table -> (
    match start_interpreting load_table with
    | Error m -> print_endline m ; Hashtbl.clear load_table
    | Ok res_context ->
        print_endline (show_context res_context ^ "\n") ;
        Hashtbl.clear load_table )

let () = print_string "--- Assign test ----\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
                 public class Program {

                     public static void Main() {
                         int a = 1;
                         int b = 2;
                         int c = 3;
                     }
                 }
                 |} )

let () = test_interp parse_input (Hashtbl.create 128)
let () = print_string "--- Arithmetic test ---\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
                       public class Program {
                           public static void Main() {
                              int a = 1;
                              int b = 2;
                              int c = 3;
                              int v1 = 11 + 2 + 7 + 41 + 15;
                              int v2 = a + b;
                              int v3 = a + 13;
                              int v4 = 10 / 3;
                              int v5 = 10 % 3;
                              int v6 = (a + b) * 100;
                              a = a + 9;
                              int v7 = a / 3;
                              int v8 = a % 3;
                              int v9 = (v1 * v2 + 4) / 2 + 100;
                              string s1 = "a";
                              string s2 = "b";
                              string s3 = s1 + s2;
                              string s4 = s1 + a;
                              string s5 = a + s1;
                           }
                       }
                       |} )

let () = test_interp parse_input (Hashtbl.create 128)
let () = print_string "--- Boolean expression test ---\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
                       public class Program {

                          public static void Main() {
                              int v1 = 0, v2 = 0, v3 = 0;
                              int v4 = 0, v5 = 0, v6 = 0, v7 = 0;
                              int v8 = 0, v9 = 0;

                              int a = 1, b = 50, c = 100;
                              if (b > a) {
                                  v1 = 1;
                              }
                              if (a < c) {
                                  v2 = 1;
                              }
                              if (b <= c) {
                                  v3 = 1;
                              }
                              if (c >= b) {
                                  v4 = 1;
                              }
                              int d = 10;
                              if (d == a) {
                                  v5 = 1;
                              }
                              if (a != 2) {
                                  v6 = 1;
                              }
                              if (a < b && b < c) {
                                  v7 = 1;
                              }
                              if (a < b || a == 10) {
                                  v8 = 1;
                              }
                              if (!(a >= c)) {
                                  v9 = 1;
                              }
                              string s1 = "a", s2 = "b";
                              int v10, v11;
                              if (s1 == "a") {
                                  v10 = 1;
                              }
                              if (s2 != "a") {
                                  v11 = 1;
                              }
                       }
                    }
                     |} )

let () = test_interp parse_input (Hashtbl.create 128)
let () = print_string "--- Cycles test ---\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
                       public class Program
                   {

                       static void Main()
                       {
                          for (int i = 0; i < 3; i++)
                          {
                            for (int j = 0; j < 2; j++)
                            {
                              Console.WriteLine(i*j);
                            }
                          }

                       }
                 }


                       |} )

let () = test_interp parse_input (Hashtbl.create 128)
let () = print_string "--- While test ---\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
                       public class Program
                   {

                       static void Main()
                       {
                          int i = 4;
                           while (i > 0)
                           {
                               Console.WriteLine(i);
                               i--;
                           }

                       }
                 }


                       |} )

let () = test_interp parse_input (Hashtbl.create 128)
let () = print_string "--- If test ---\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
                       public class Program
                   {

                       static void Main()
                       {
                          int a = 8;
                          int b = 6;
                          if (a > b)
                          {
                              Console.WriteLine("a");
                          }

                       }
                 }


                       |} )

let () = test_interp parse_input (Hashtbl.create 128)
let () = print_string "--- If else if test ---\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
                       public class Program
                   {

                       static void Main()
                       {
                          int a = 6;
                           int b = 6;
                           if(a > b)
                           {
                               Console.WriteLine("a>b");
                           }
                           else if (a < b)
                           {
                               Console.WriteLine("a<b");
                           }
                           else
                           {
                               Console.WriteLine("a==b");
                           }

                       }
                 }
                       |} )

let () = test_interp parse_input (Hashtbl.create 128)
let () = print_string "--- Visibility level test ---\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
                          public class Program
                         {
                             static void Main()
                             {
                                 int a = 12;
                                 int b = 13;
                                 int c = 14;
                                 if (c == 14) {
                                     int d = 7;
                                     a = a + 1;
                                     int e = 10;
                                     int g = 42;
                                 }
                                 b = b + 1;
                                 c = c + 1;
                                 a = a + 1;
                                 int i = 0;
                                 while (i < 3) {
                                   int m = 2;
                                   int n = 3;
                                   int z = 4;
                                   i = i + 1;
                                   c = c + 1;
                                 }
                                 a = 100;
                                 b = 200;
                                 c = 300;
                                 for (int k = 0, p = 0; k < 6; k++) {
                                     int m = 2;
                                     int n = 3;
                                     int z = 4;
                                     a = a + 1;
                                 }
                                 a = 1;
                                 b = 2;
                                 c = 3;
                             }
                         }
                          |} )

let () = test_interp parse_input (Hashtbl.create 128)
let () = print_string "--- Break test ---\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
                          public class Program
                      {

                          static void Main()
                          {
                             for (int i = 0; i < 4; i++)
                            {
                                if (i == 2)
                                    break;
                                Console.WriteLine(i);
                            }

                          }
                    }


                          |} )

let () = test_interp parse_input (Hashtbl.create 128)
let () = print_string "--- Continue test ---\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
                          public class Program
                      {

                          static void Main()
                          {
                             for (int i = 0; i < 4; i++)
                            {
                                if (i == 2)
                                    continue;
                                Console.WriteLine(i);
                            }

                          }
                    }


                          |} )

let () = test_interp parse_input (Hashtbl.create 128)
let () = print_string "--- Recursion test ---\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
                          public class Program
                      {

                          static void Main()
                          {
                          int res = Factorial(5);
                          }
                          static int Factorial(int x)
                          {
                              if (x == 0)
                              {
                                  return 1;
                              }
                              else
                              {
                                  return x * Factorial(x - 1);
                              }
                          }

                    }


                          |} )

let () = test_interp parse_input (Hashtbl.create 128)
let () = print_string "--- Const variables test ---\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
                      public class Program
                      {
                      static void Main()
                          {
                            const int pi = 4;
                            pi = 3;
                          }
                     }
                          |} )

let () = test_interp parse_input (Hashtbl.create 128)
