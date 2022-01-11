open Csharp_lib.Parser

open Csharp_lib.Interpreter.ClassLoader (Csharp_lib.Interpreter.Result)

open Csharp_lib.Interpreter.Interpreter (Csharp_lib.Interpreter.Result)

let test_interp test_val cl_t =
  match load_classes test_val cl_t with
  | Error m ->
      print_endline m;
      Hashtbl.clear cl_t
  | Ok load_table -> (
      match start_interpreting load_table with
      | Error m ->
          print_endline m;
          Hashtbl.clear load_table
      | Ok _ -> Hashtbl.clear load_table)

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
        |})

let () = test_interp parse_input (Hashtbl.create 128)

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
        |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "--- DemoException test in Main try-catch(e) with finally ---\n\n"

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
                     catch (Exception e)
                     {
                       Console.WriteLine("Handled");
                     }
                     finally
                     {
                       Console.WriteLine("In finally");
                     }

               }
           }
           |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "--- DemoException test in Main try-catch(e) with finally and external \
     method throw exception ---\n\n"

let parse_input =
  Option.get
    (apply_parser parser
       {|
           public class Program {

               public static void Main() {
                  int a = 1;
                   try
                     {
                       FThrow();
                     }
                     catch (Exception e)
                     {
                       Console.WriteLine(a);
                     }
                     finally
                     {
                       Console.WriteLine("In finally");
                     }

               }

               public static void FThrow()
               {
                     throw new Exception();
               }
           }
           |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "--- DemoException test in Main try-catch(e) with finally (unhandled \
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
           |})

let () = test_interp parse_input (Hashtbl.create 128)

let () =
  print_string
    "--- DemoException test in changeA try-catch(e) with finally ---\n\n"

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
           |})

let () = test_interp parse_input (Hashtbl.create 128)
