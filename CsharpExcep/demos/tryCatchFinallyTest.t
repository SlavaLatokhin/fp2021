Try-catch-finally validation tests
Test 1 (handled and in finally)
  $ ./demoOnlyPrint.exe <<-EOF
  >      public class Program {
  >             public static void Main() {
  >                   try
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
  
Test2 (without finally)
  $ ./demoOnlyPrint.exe <<-EOF
  >   public class Program {
  >           public static void Main() {
  >                   try
  >                   {
  >                     throw new Exception();
  >                   }
  >                   catch (Exception)
  >                   {
  >                     Console.WriteLine("Handled");
  >                   }
  >             }
  >    }
  > EOF
  Handled
  
Test3 (with method)
  $ ./demoOnlyPrint.exe <<-EOF
  >     public class Program {
  >             public static void Main() {
  >                     try
  >                     {
  >                        Thqrow();
  >                     }
  >                     catch
  >                     {
  >                       Console.WriteLine("Handled");
  >                     }
  >                     finally
  >                     {
  >                       Console.WriteLine("In finally");
  >                     }
  >                }
  >                public static void Thqrow()
  >                {
  >                      throw new Exception();
  >                }
  >     }
  > EOF
  Handled
  In finally
  
Test4 (unhandled exception and in finally)
  $ ./demoOnlyPrint.exe <<-EOF
  >     public class Program {
  >             public static void Main() {
  >                     try
  >                     {
  >                        Thqrow();
  >                     }
  >                     catch(Sth)
  >                     {
  >                     }
  >                     finally
  >                     {
  >                       Console.WriteLine("In finally");
  >                     }
  >                }
  >                public static void Thqrow()
  >                {
  >                      throw new Exception();
  >                }
  >     }
  > EOF
  In finally
  Unhandled exception

Test5 (finally with return)
  $ ./demoOnlyPrint.exe <<-EOF
  >        public class Program {
  >            static void Main()
  >            {
  >              Console.WriteLine(changeA());
  >            }
  >            static int changeA() {
  >                int a = 5;
  >                try 
  >                {
  >                    return a;
  >                }
  >                finally 
  >                {
  >                    a = 200;
  >                    Console.WriteLine(a);    
  >                }
  >            }
  >        }
  > EOF
  200
  5
  
Test6 (several exceptions)
  $ ./demoOnlyPrint.exe <<-EOF
  >                 public class Program {
  >                    public static void Main() {
  >                          try
  >                          {
  >                            Foo();
  >                          }
  >                          catch (SecondException) {
  >                            Console.WriteLine("Handled SecondException");
  >                          }
  >                          catch (FirstException) {
  >                            Console.WriteLine("Handled FirstException");
  >                          }
  >                          finally
  >                          {
  >                            Console.WriteLine("In finally");
  >                          }
  >                    }  
  >                    public static void Foo()
  >                    {
  >                          throw new FirstException();
  >                    }
  >                }
  >                class FirstException : Exception
  >                {          
  >             
  >                }
  >                class SecondException : Exception
  >                {
  >                        
  >                }
  > EOF
  Handled FirstException
  In finally
  
Test7 (handled some exception)
  $ ./demoOnlyPrint.exe <<-EOF
  >                 public class Program {
  >                   public static void Main() {
  >                        try
  >                          {
  >                            Foo();
  >                          }
  >                          catch (Exception) {
  >                            Console.WriteLine("Handled Exception");
  >                          }
  >                          finally
  >                          {
  >                            Console.WriteLine("In finally");
  >                          }
  >                    }  
  >                    public static void Foo()
  >                    {
  >                          throw new FirstException();
  >                    }
  >                }
  >                class FirstException : Exception
  >                {
  >                        
  >                }
  > EOF
  Handled Exception
  In finally
  
