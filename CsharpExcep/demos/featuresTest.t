Features tests
Test 1 (Assign test)
  $ ./demoShowContext.exe <<-EOF
  >       public class Program {
  >               public static void Main() {
  >                    int a = 1;
  >                    int b = 2;
  >                    int c = 3;
  >               }
  >       }
  > EOF      
  { variable_map =
    ["a": { var_type = Int; var_key = "a"; var_value = (VInt 1);
            is_const = false; assignment_count = 1; visibility_level = 0 },
     "b": { var_type = Int; var_key = "b"; var_value = (VInt 2);
            is_const = false; assignment_count = 1; visibility_level = 0 },
     "c": { var_type = Int; var_key = "c"; var_value = (VInt 3);
            is_const = false; assignment_count = 1; visibility_level = 0 },
     ];
    current_method_type = Void; last_expr_result = (VInt 3);
    runtime_signal = NoSignal; count_of_nested_cycles = 0;
    visibility_level = 0; post_inc = []; post_dec = [] }
  

Test 2 (Arithmetic operations test)
  $ ./demoShowContext.exe <<-EOF
  >       public class Program {
  >              public static void Main() {
  >                    int a = 1;
  >                    int b = 2;
  >                    int c = 3;
  >                    int v1 = 11 + 2 + 7 + 41 + 15;
  >                    int v2 = a + b;
  >                    int v3 = a + 13;
  >                    int v4 = 10 / 3;
  >                    int v5 = 10 % 3;
  >                    int v6 = (a + b) * 100;
  >                    a = a + 9;
  >                    int v7 = a / 3;
  >                    int v8 = a % 3;
  >                    int v9 = (v1 * v2 + 4) / 2 + 100 * 3;
  >                    string s1 = "a";
  >                    string s2 = "b";
  >                    string s3 = s1 + s2;
  >                    string s4 = s1 + a;
  >                    string s5 = a + s1;
  >                    a = 5;
  >                    int v10 = a++;
  >                    Console.WriteLine(a);
  >                    a = 5;
  >                    int v11 = a--;
  >                    Console.WriteLine(a);
  >                    a = 5;  
  >                    int v12 = ++a;
  >                    Console.WriteLine(a);
  >                    a = 5;  
  >                    int v13 = --a;
  >                    Console.WriteLine(a);
  >                    int a = 5;
  >                    int b = a++ + a++ + 9 + a++;
  >                    Console.WriteLine(b);
  >                    Console.WriteLine(a);
  >              }
  >        }
  > EOF
  6
  4
  6
  4
  27
  8
  { variable_map =
    ["a": { var_type = Int; var_key = "a"; var_value = (VInt 8);
            is_const = false; assignment_count = 7; visibility_level = 0 },
     "b": { var_type = Int; var_key = "b"; var_value = (VInt 27);
            is_const = false; assignment_count = 1; visibility_level = 0 },
     "c": { var_type = Int; var_key = "c"; var_value = (VInt 3);
            is_const = false; assignment_count = 1; visibility_level = 0 },
     "s1": { var_type = String; var_key = "s1"; var_value = (VString "a");
             is_const = false; assignment_count = 1; visibility_level = 0 },
     "s2": { var_type = String; var_key = "s2"; var_value = (VString "b");
             is_const = false; assignment_count = 1; visibility_level = 0 },
     "s3": { var_type = String; var_key = "s3"; var_value = (VString "ab");
             is_const = false; assignment_count = 1; visibility_level = 0 },
     "s4": { var_type = String; var_key = "s4"; var_value = (VString "a10");
             is_const = false; assignment_count = 1; visibility_level = 0 },
     "s5": { var_type = String; var_key = "s5"; var_value = (VString "10a");
             is_const = false; assignment_count = 1; visibility_level = 0 },
     "v1": { var_type = Int; var_key = "v1"; var_value = (VInt 76);
             is_const = false; assignment_count = 1; visibility_level = 0 },
     "v10": { var_type = Int; var_key = "v10"; var_value = (VInt 5);
              is_const = false; assignment_count = 1; visibility_level = 0 },
     "v11": { var_type = Int; var_key = "v11"; var_value = (VInt 5);
              is_const = false; assignment_count = 1; visibility_level = 0 },
     "v12": { var_type = Int; var_key = "v12"; var_value = (VInt 6);
              is_const = false; assignment_count = 1; visibility_level = 0 },
     "v13": { var_type = Int; var_key = "v13"; var_value = (VInt 4);
              is_const = false; assignment_count = 1; visibility_level = 0 },
     "v2": { var_type = Int; var_key = "v2"; var_value = (VInt 3);
             is_const = false; assignment_count = 1; visibility_level = 0 },
     "v3": { var_type = Int; var_key = "v3"; var_value = (VInt 14);
             is_const = false; assignment_count = 1; visibility_level = 0 },
     "v4": { var_type = Int; var_key = "v4"; var_value = (VInt 3);
             is_const = false; assignment_count = 1; visibility_level = 0 },
     "v5": { var_type = Int; var_key = "v5"; var_value = (VInt 1);
             is_const = false; assignment_count = 1; visibility_level = 0 },
     "v6": { var_type = Int; var_key = "v6"; var_value = (VInt 300);
             is_const = false; assignment_count = 1; visibility_level = 0 },
     "v7": { var_type = Int; var_key = "v7"; var_value = (VInt 3);
             is_const = false; assignment_count = 1; visibility_level = 0 },
     "v8": { var_type = Int; var_key = "v8"; var_value = (VInt 1);
             is_const = false; assignment_count = 1; visibility_level = 0 },
     "v9": { var_type = Int; var_key = "v9"; var_value = (VInt 416);
             is_const = false; assignment_count = 1; visibility_level = 0 },
     ];
    current_method_type = Void; last_expr_result = (VInt 8);
    runtime_signal = NoSignal; count_of_nested_cycles = 0;
    visibility_level = 0; post_inc = []; post_dec = [] }
  

Test 3 (Boolean expression test)
  $ ./demoShowContext.exe <<-EOF
  >      public class Program {
  >          public static void Main() {
  >                int v1 = 0, v2 = 0, v3 = 0;
  >                int v4 = 0, v5 = 0, v6 = 0, v7 = 0;
  >                int v8 = 0, v9 = 0;
  >                int a = 1, b = 50, c = 100;
  >                if (b > a) {
  >                    v1 = 1;
  >                }
  >                if (a < c) {
  >                    v2 = 1;
  >                }
  >                if (b <= c) {
  >                    v3 = 1;
  >                }
  >                if (c >= b) {
  >                    v4 = 1;
  >                }
  >                int d = 10;
  >                if (d == a) {
  >                    v5 = 1;
  >                }
  >                if (a != 2) {
  >                    v6 = 1;
  >                }
  >                if (a < b && b < c) {
  >                   v7 = 1;
  >                }
  >                if (a < b || a == 10) {
  >                    v8 = 1;
  >                }
  >                if (!(a >= c)) {
  >                    v9 = 1;
  >                }
  >                string s1 = "a", s2 = "b";
  >                int v10, v11;
  >                if (s1 == "a") {
  >                    v10 = 1;
  >                }
  >                if (s2 != "a") {
  >                    v11 = 1;
  >                }
  >           }
  >      }
  > EOF  
  { variable_map =
    ["a": { var_type = Int; var_key = "a"; var_value = (VInt 1);
            is_const = false; assignment_count = 1; visibility_level = 0 },
     "b": { var_type = Int; var_key = "b"; var_value = (VInt 50);
            is_const = false; assignment_count = 1; visibility_level = 0 },
     "c": { var_type = Int; var_key = "c"; var_value = (VInt 100);
            is_const = false; assignment_count = 1; visibility_level = 0 },
     "d": { var_type = Int; var_key = "d"; var_value = (VInt 10);
            is_const = false; assignment_count = 1; visibility_level = 0 },
     "s1": { var_type = String; var_key = "s1"; var_value = (VString "a");
             is_const = false; assignment_count = 1; visibility_level = 0 },
     "s2": { var_type = String; var_key = "s2"; var_value = (VString "b");
             is_const = false; assignment_count = 1; visibility_level = 0 },
     "v1": { var_type = Int; var_key = "v1"; var_value = (VInt 1);
             is_const = false; assignment_count = 2; visibility_level = 0 },
     "v10": { var_type = Int; var_key = "v10"; var_value = (VInt 1);
              is_const = false; assignment_count = 1; visibility_level = 0 },
     "v11": { var_type = Int; var_key = "v11"; var_value = (VInt 1);
              is_const = false; assignment_count = 1; visibility_level = 0 },
     "v2": { var_type = Int; var_key = "v2"; var_value = (VInt 1);
             is_const = false; assignment_count = 2; visibility_level = 0 },
     "v3": { var_type = Int; var_key = "v3"; var_value = (VInt 1);
             is_const = false; assignment_count = 2; visibility_level = 0 },
     "v4": { var_type = Int; var_key = "v4"; var_value = (VInt 1);
             is_const = false; assignment_count = 2; visibility_level = 0 },
     "v5": { var_type = Int; var_key = "v5"; var_value = (VInt 0);
             is_const = false; assignment_count = 1; visibility_level = 0 },
     "v6": { var_type = Int; var_key = "v6"; var_value = (VInt 1);
             is_const = false; assignment_count = 2; visibility_level = 0 },
     "v7": { var_type = Int; var_key = "v7"; var_value = (VInt 1);
             is_const = false; assignment_count = 2; visibility_level = 0 },
     "v8": { var_type = Int; var_key = "v8"; var_value = (VInt 1);
             is_const = false; assignment_count = 2; visibility_level = 0 },
     "v9": { var_type = Int; var_key = "v9"; var_value = (VInt 1);
             is_const = false; assignment_count = 2; visibility_level = 0 },
     ];
    current_method_type = Void; last_expr_result = (VInt 1);
    runtime_signal = NoSignal; count_of_nested_cycles = 0;
    visibility_level = 0; post_inc = []; post_dec = [] }
  

Test 4 (For loop test)
  $ ./demoShowContext.exe <<-EOF
  >            public class Program {
  >                 static void Main() {
  >                       for (int i = 0; i < 3; i++) {
  >                          for (int j = 0; j < 2; j++) {
  >                              Console.WriteLine(i*j);
  >                          }
  >                       }
  >                 }
  >            }
  > EOF 
  0
  0
  0
  1
  0
  2
  { variable_map =
    ["i": { var_type = Int; var_key = "i"; var_value = (VInt 3);
            is_const = false; assignment_count = 7; visibility_level = 1 },
     ];
    current_method_type = Void; last_expr_result = (VBool false);
    runtime_signal = NoSignal; count_of_nested_cycles = 0;
    visibility_level = 2; post_inc = []; post_dec = [] }
  

Test 5 (While loop test)
  $ ./demoShowContext.exe <<-EOF
  >     public class Program {
  >          static void Main() {
  >                 int i = 4;
  >                 while (i > 0)
  >                 {
  >                     Console.WriteLine(i);
  >                     i--;
  >                 }
  >           }
  >      }
  > EOF  
  4
  3
  2
  1
  { variable_map =
    ["i": { var_type = Int; var_key = "i"; var_value = (VInt 0);
            is_const = false; assignment_count = 9; visibility_level = 0 },
     ];
    current_method_type = Void; last_expr_result = (VBool false);
    runtime_signal = NoSignal; count_of_nested_cycles = 0;
    visibility_level = 0; post_inc = []; post_dec = [] }
  

Test 5 (If test)
  $ ./demoShowContext.exe <<-EOF
  >       public class Program {
  >             static void Main() {
  >                    int a = 8;
  >                    int b = 6;
  >                    if (a > b) {
  >                        Console.WriteLine("a");
  >                    }
  >             }
  >       }
  > EOF   
  a
  { variable_map =
    ["a": { var_type = Int; var_key = "a"; var_value = (VInt 8);
            is_const = false; assignment_count = 1; visibility_level = 0 },
     "b": { var_type = Int; var_key = "b"; var_value = (VInt 6);
            is_const = false; assignment_count = 1; visibility_level = 0 },
     ];
    current_method_type = Void; last_expr_result = (VString "a");
    runtime_signal = NoSignal; count_of_nested_cycles = 0;
    visibility_level = 0; post_inc = []; post_dec = [] }
  

Test 6 (If else if test)
  $ ./demoShowContext.exe <<-EOF
  >        public class Program {
  >              static void Main() {
  >                  int a = 6;
  >                  int b = 6;
  >                  if(a > b)
  >                  {
  >                     Console.WriteLine("a>b");
  >                  }
  >                  else if (a < b)
  >                  {
  >                     Console.WriteLine("a<b");
  >                  }
  >                  else
  >                  {
  >                      Console.WriteLine("a==b");
  >                  }
  >              }
  >         }
  > EOF  
  a==b
  { variable_map =
    ["a": { var_type = Int; var_key = "a"; var_value = (VInt 6);
            is_const = false; assignment_count = 1; visibility_level = 0 },
     "b": { var_type = Int; var_key = "b"; var_value = (VInt 6);
            is_const = false; assignment_count = 1; visibility_level = 0 },
     ];
    current_method_type = Void; last_expr_result = (VString "a==b");
    runtime_signal = NoSignal; count_of_nested_cycles = 0;
    visibility_level = 0; post_inc = []; post_dec = [] }
  

Test 8 (Visibility level test)
  $ ./demoShowContext.exe <<-EOF
  >      public class Program
  >      {
  >            static void Main()
  >            {
  >                  int a = 12;
  >                  int b = 13;
  >                  int c = 14;
  >                  if (c == 14) {
  >                      int m = 7;
  >                      a = a + 1;
  >                  }
  >                  b = b + 1;
  >                  c = c + 1;
  >                  a = a + 1;
  >                  int i = 0;
  >                  while (i < 3) {
  >                      int m = 2;
  >                      int n = 3;
  >                      m = m + n;
  >                      b = m + i;
  >                      i = i + 1;
  >                  }
  >                  a = 100;
  >                  b = 200;
  >                  c = 300;
  >                  for (int k = 0, p = 0; k < 6; k++) {
  >                       int m = 2;
  >                       int n = 3;
  >                       int z = 4;
  >                       a = a + 1;
  >                  }
  >                  a = 1;
  >                  b = 2;
  >                  c = 3;
  >           }
  >     }
  > EOF  
  { variable_map =
    ["a": { var_type = Int; var_key = "a"; var_value = (VInt 1);
            is_const = false; assignment_count = 11; visibility_level = 0 },
     "b": { var_type = Int; var_key = "b"; var_value = (VInt 2);
            is_const = false; assignment_count = 7; visibility_level = 0 },
     "c": { var_type = Int; var_key = "c"; var_value = (VInt 3);
            is_const = false; assignment_count = 4; visibility_level = 0 },
     "i": { var_type = Int; var_key = "i"; var_value = (VInt 3);
            is_const = false; assignment_count = 4; visibility_level = 0 },
     "m": { var_type = Int; var_key = "m"; var_value = (VInt 2);
            is_const = false; assignment_count = 1; visibility_level = 2 },
     "n": { var_type = Int; var_key = "n"; var_value = (VInt 3);
            is_const = false; assignment_count = 1; visibility_level = 2 },
     "z": { var_type = Int; var_key = "z"; var_value = (VInt 4);
            is_const = false; assignment_count = 1; visibility_level = 2 },
     ];
    current_method_type = Void; last_expr_result = (VInt 3);
    runtime_signal = NoSignal; count_of_nested_cycles = 0;
    visibility_level = 1; post_inc = []; post_dec = [] }
  

Test 9 (Break test)
  $ ./demoShowContext.exe <<-EOF
  >        public class Program
  >        {
  >             static void Main()
  >             {
  >                 for (int i = 0; i < 4; i++)
  >                 {
  >                      if (i == 2)
  >                         break;
  >                      Console.WriteLine(i);
  >                  }
  >             }
  >        }
  > EOF 
  0
  1
  { variable_map = []; current_method_type = Void; last_expr_result = (VInt 1);
    runtime_signal = NoSignal; count_of_nested_cycles = 0;
    visibility_level = 1; post_inc = []; post_dec = [] }
  

Test 10 (Continue test)
  $ ./demoShowContext.exe <<-EOF
  >         public class Program
  >         {
  >              static void Main()
  >              {
  >                  for (int i = 0; i < 4; i++)
  >                  {
  >                       if (i == 2)
  >                          continue;
  >                       Console.WriteLine(i);
  >                   }
  >              }
  >         }
  > EOF
  0
  1
  3
  { variable_map = []; current_method_type = Void;
    last_expr_result = (VBool false); runtime_signal = NoSignal;
    count_of_nested_cycles = 0; visibility_level = 1; post_inc = [];
    post_dec = [] }
  

Test 11 (Recursion test)
  $ ./demoShowContext.exe <<-EOF
  >        public class Program
  >        {
  >          static void Main()
  >          {
  >            int res = Factorial(5);
  >          }
  >          static int Factorial(int x)
  >          {
  >              if (x == 0)
  >              {
  >                  return 1;
  >              }
  >              else
  >              {
  >                  return x * Factorial(x - 1);
  >              }
  >          }
  >       }
  > EOF
  { variable_map =
    ["res": { var_type = Int; var_key = "res"; var_value = (VInt 120);
              is_const = false; assignment_count = 1; visibility_level = 0 },
     "x": { var_type = Int; var_key = "x"; var_value = (VInt 5);
            is_const = false; assignment_count = 1; visibility_level = 0 },
     ];
    current_method_type = Void; last_expr_result = (VInt 120);
    runtime_signal = NoSignal; count_of_nested_cycles = 0;
    visibility_level = 0; post_inc = []; post_dec = [] }
  

Test 12 (Const variables test)
  $ ./demoShowContext.exe <<-EOF
  >        public class Program
  >        {
  >            static void Main()
  >            {
  >                const int pi = 4;
  >                pi = 3;
  >            }
  >        }
  > EOF 
  Assigment to a constant variable
