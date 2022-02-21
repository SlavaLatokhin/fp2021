  $ ./demo.exe <<-EOF
  >      public class Program {
  >             public static void Main() {
  >                 try
  >                   {
  >                     throw new Exception();
  >                   }
  >                   catch
  >                   {
  >                     Console.WriteLine("Handled");
  >                   }
  >                   finally
  >                   {
  >                     Console.WriteLine("In finally");
  >                   }
  >             }
  >         }
  > EOF
  Handled
  In finally
  

