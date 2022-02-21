open Csharp_lib.Parser

open
  Csharp_lib.Interpret_classes.Interpret_classes
    (Csharp_lib.Interpret_classes.Result)

open Csharp_lib.Interpreter.Interpreter (Csharp_lib.Interpret_classes.Result)

let test_interp class_list_ast class_list =
  match interpret_classes class_list_ast class_list with
  | Error m -> print_endline m
  | Ok load_list -> (
    match start_interpreting load_list with
    | Error m -> print_endline m
    | Ok _ -> print_endline "" )

let () =
  print_string "--- DemoException test in Main try-catch with finally ---\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
           public class Program {

               public static void Main() {
                   try
                     {
                       throw new Exception();
                     }
                     catch
                     {
                       Console.WriteLine("Handled");
                     }
                     finally
                     {
                       Console.WriteLine("In finally");
                     }
               }
           }
           |} )

let () = test_interp parse_input []

let () =
  print_string
    "--- DemoException test in Main try-catch without finally ---\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
           public class Program {

               public static void Main() {
                   try
                     {
                       throw new Exception();
                     }
                     catch (Exception)
                     {
                       Console.WriteLine("Handled");
                     }

               }
           }
           |} )

let () = test_interp parse_input []

let () =
  print_string
    "--- DemoException test in Main try-catch() with finally (unhandled \
     exception) ---\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
              public class Program {

                  public static void Main() {
                      try
                        {
                          Thqrow();
                        }
                       catch(Sth)
                       {

                       }
                       finally
                       {
                         Console.WriteLine("In finally");
                       }
                  }

                  public static void Thqrow()
                  {
                        throw new Exception();
                  }
              }
              |} )

let () = test_interp parse_input []

let () =
  print_string
    "--- DemoException test in changeA try-catch() with finally ---\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
              public class Program {

                  static void Main()
              {
                  Console.WriteLine(changeA());
              }

              static int changeA() {
                  int a = 5;
                  try {
                      return a;
                  } finally {
                      a = 200;
                  }
              }
              }
              |} )

let () = test_interp parse_input []

let () =
  print_string
    "--- DemoException test inheritance of Exception and try-catch() ---\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
                  public class Program {
    
                      public static void Main() {
                          try
                            {
                              Foo();
                            }
                            catch (SecondException) {
                              Console.WriteLine("Catched SecondException");
                            }
                            catch (FirstException) {
                              Console.WriteLine("Catched FirstException");
                            }
                            finally
                            {
                              Console.WriteLine("In finally");
                            }
                      }
    
                      public static void Foo()
                      {
                            throw new FirstException();
                      }

                  }

                  class FirstException : Exception
                  {
                          
                  }
                  class SecondException : Exception
                  {
                          
                  }
                  |} )

let () = test_interp parse_input []

let () =
  print_string
    "--- DemoException test inheritance of Exception and try-catch() 2 ---\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
                  public class Program {
    
                      public static void Main() {
                          try
                            {
                              Foo();
                            }
                            catch (Exception) {
                              Console.WriteLine("Catched Exception");
                            }
                            finally
                            {
                              Console.WriteLine("In finally");
                            }
                      }
    
                      public static void Foo()
                      {
                            throw new FirstException();
                      }

                  }

                  class FirstException : Exception
                  {
                          
                  }
                  class SecondException : Exception
                  {
                          
                  }
                  |} )

let () = test_interp parse_input []
