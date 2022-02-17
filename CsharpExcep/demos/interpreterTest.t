  $ (cd ../../../../default && demos/demoInterpreter.exe) 
  --- Assign test ----
  
  { variable_table =
    [["a" ->
       { var_type = Int; var_key = "a"; var_value = VInt (1); is_const = false;
         assignment_count = 1; visibility_level = 0 }
    
  "b" ->
   { var_type = Int; var_key = "b"; var_value = VInt (2); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "c" ->
   { var_type = Int; var_key = "c"; var_value = VInt (3); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  ]]
  ; current_method_type = Void; last_expr_result = VInt (3);
  runtime_signal = NoSignal; count_of_nested_cycles = 0; visibility_level = 0
  }
  
  --- Arithmetic test ---
  
  { variable_table =
    [["a" ->
       { var_type = Int; var_key = "a"; var_value = VInt (10);
         is_const = false; assignment_count = 2; visibility_level = 0 }
    
  "v1" ->
   { var_type = Int; var_key = "v1"; var_value = VInt (76); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "b" ->
   { var_type = Int; var_key = "b"; var_value = VInt (2); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "s5" ->
   { var_type = String; var_key = "s5"; var_value = VString ("10a");
     is_const = false; assignment_count = 1; visibility_level = 0 }
  
  "v5" ->
   { var_type = Int; var_key = "v5"; var_value = VInt (1); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "v4" ->
   { var_type = Int; var_key = "v4"; var_value = VInt (3); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "v2" ->
   { var_type = Int; var_key = "v2"; var_value = VInt (3); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "s1" ->
   { var_type = String; var_key = "s1"; var_value = VString ("a");
     is_const = false; assignment_count = 1; visibility_level = 0 }
  
  "s3" ->
   { var_type = String; var_key = "s3"; var_value = VString ("ab");
     is_const = false; assignment_count = 1; visibility_level = 0 }
  
  "v6" ->
   { var_type = Int; var_key = "v6"; var_value = VInt (300); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "s4" ->
   { var_type = String; var_key = "s4"; var_value = VString ("a10");
     is_const = false; assignment_count = 1; visibility_level = 0 }
  
  "v9" ->
   { var_type = Int; var_key = "v9"; var_value = VInt (216); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "v8" ->
   { var_type = Int; var_key = "v8"; var_value = VInt (1); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "s2" ->
   { var_type = String; var_key = "s2"; var_value = VString ("b");
     is_const = false; assignment_count = 1; visibility_level = 0 }
  
  "v7" ->
   { var_type = Int; var_key = "v7"; var_value = VInt (3); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "v3" ->
   { var_type = Int; var_key = "v3"; var_value = VInt (14); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "c" ->
   { var_type = Int; var_key = "c"; var_value = VInt (3); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  ]]
  ; current_method_type = Void; last_expr_result = VString ("10a");
  runtime_signal = NoSignal; count_of_nested_cycles = 0; visibility_level = 0
  }
  
  --- Boolean expression test ---
  
  { variable_table =
    [["v11" ->
       { var_type = Int; var_key = "v11"; var_value = VInt (1);
         is_const = false; assignment_count = 1; visibility_level = 0 }
    
  "a" ->
   { var_type = Int; var_key = "a"; var_value = VInt (1); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "v1" ->
   { var_type = Int; var_key = "v1"; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "b" ->
   { var_type = Int; var_key = "b"; var_value = VInt (50); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "v5" ->
   { var_type = Int; var_key = "v5"; var_value = VInt (0); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "v4" ->
   { var_type = Int; var_key = "v4"; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "v2" ->
   { var_type = Int; var_key = "v2"; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "s1" ->
   { var_type = String; var_key = "s1"; var_value = VString ("a");
     is_const = false; assignment_count = 1; visibility_level = 0 }
  
  "d" ->
   { var_type = Int; var_key = "d"; var_value = VInt (10); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "v6" ->
   { var_type = Int; var_key = "v6"; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "v9" ->
   { var_type = Int; var_key = "v9"; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "v8" ->
   { var_type = Int; var_key = "v8"; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "s2" ->
   { var_type = String; var_key = "s2"; var_value = VString ("b");
     is_const = false; assignment_count = 1; visibility_level = 0 }
  
  "v7" ->
   { var_type = Int; var_key = "v7"; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "v3" ->
   { var_type = Int; var_key = "v3"; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "v10" ->
   { var_type = Int; var_key = "v10"; var_value = VInt (1); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "c" ->
   { var_type = Int; var_key = "c"; var_value = VInt (100); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  ]]
  ; current_method_type = Void; last_expr_result = VInt (1);
  runtime_signal = NoSignal; count_of_nested_cycles = 0; visibility_level = 0
  }
  
  --- Cycles test ---
  
  0
  0
  0
  1
  0
  2
  { variable_table =
    [["i" ->
       { var_type = Int; var_key = "i"; var_value = VInt (3); is_const = false;
         assignment_count = 4; visibility_level = 1 }
    
  ]]
  ; current_method_type = Void; last_expr_result = VBool (false);
  runtime_signal = NoSignal; count_of_nested_cycles = 0; visibility_level = 2
  }
  
  --- While test ---
  
  4
  3
  2
  1
  { variable_table =
    [["i" ->
       { var_type = Int; var_key = "i"; var_value = VInt (0); is_const = false;
         assignment_count = 5; visibility_level = 0 }
    
  ]]
  ; current_method_type = Void; last_expr_result = VBool (false);
  runtime_signal = NoSignal; count_of_nested_cycles = 0; visibility_level = 0
  }
  
  --- If test ---
  
  a
  { variable_table =
    [["a" ->
       { var_type = Int; var_key = "a"; var_value = VInt (8); is_const = false;
         assignment_count = 1; visibility_level = 0 }
    
  "b" ->
   { var_type = Int; var_key = "b"; var_value = VInt (6); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  ]]
  ; current_method_type = Void; last_expr_result = VString ("a");
  runtime_signal = NoSignal; count_of_nested_cycles = 0; visibility_level = 0
  }
  
  --- If else if test ---
  
  a==b
  { variable_table =
    [["a" ->
       { var_type = Int; var_key = "a"; var_value = VInt (6); is_const = false;
         assignment_count = 1; visibility_level = 0 }
    
  "b" ->
   { var_type = Int; var_key = "b"; var_value = VInt (6); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  ]]
  ; current_method_type = Void; last_expr_result = VString ("a==b");
  runtime_signal = NoSignal; count_of_nested_cycles = 0; visibility_level = 0
  }
  
  --- Visibility level test ---
  
  { variable_table =
    [["a" ->
       { var_type = Int; var_key = "a"; var_value = VInt (1); is_const = false;
         assignment_count = 11; visibility_level = 0 }
    
  "n" ->
   { var_type = Int; var_key = "n"; var_value = VInt (3); is_const = false;
     assignment_count = 1; visibility_level = 2 }
  
  "n" ->
   { var_type = Int; var_key = "n"; var_value = VInt (3); is_const = false;
     assignment_count = 1; visibility_level = 2 }
  
  "n" ->
   { var_type = Int; var_key = "n"; var_value = VInt (3); is_const = false;
     assignment_count = 1; visibility_level = 2 }
  
  "n" ->
   { var_type = Int; var_key = "n"; var_value = VInt (3); is_const = false;
     assignment_count = 1; visibility_level = 1 }
  
  "n" ->
   { var_type = Int; var_key = "n"; var_value = VInt (3); is_const = false;
     assignment_count = 1; visibility_level = 1 }
  
  "n" ->
   { var_type = Int; var_key = "n"; var_value = VInt (3); is_const = false;
     assignment_count = 1; visibility_level = 1 }
  
  "z" ->
   { var_type = Int; var_key = "z"; var_value = VInt (4); is_const = false;
     assignment_count = 1; visibility_level = 2 }
  
  "z" ->
   { var_type = Int; var_key = "z"; var_value = VInt (4); is_const = false;
     assignment_count = 1; visibility_level = 2 }
  
  "z" ->
   { var_type = Int; var_key = "z"; var_value = VInt (4); is_const = false;
     assignment_count = 1; visibility_level = 2 }
  
  "z" ->
   { var_type = Int; var_key = "z"; var_value = VInt (4); is_const = false;
     assignment_count = 1; visibility_level = 1 }
  
  "z" ->
   { var_type = Int; var_key = "z"; var_value = VInt (4); is_const = false;
     assignment_count = 1; visibility_level = 1 }
  
  "z" ->
   { var_type = Int; var_key = "z"; var_value = VInt (4); is_const = false;
     assignment_count = 1; visibility_level = 1 }
  
  "b" ->
   { var_type = Int; var_key = "b"; var_value = VInt (2); is_const = false;
     assignment_count = 4; visibility_level = 0 }
  
  "m" ->
   { var_type = Int; var_key = "m"; var_value = VInt (2); is_const = false;
     assignment_count = 1; visibility_level = 2 }
  
  "m" ->
   { var_type = Int; var_key = "m"; var_value = VInt (2); is_const = false;
     assignment_count = 1; visibility_level = 2 }
  
  "m" ->
   { var_type = Int; var_key = "m"; var_value = VInt (2); is_const = false;
     assignment_count = 1; visibility_level = 2 }
  
  "m" ->
   { var_type = Int; var_key = "m"; var_value = VInt (2); is_const = false;
     assignment_count = 1; visibility_level = 1 }
  
  "m" ->
   { var_type = Int; var_key = "m"; var_value = VInt (2); is_const = false;
     assignment_count = 1; visibility_level = 1 }
  
  "m" ->
   { var_type = Int; var_key = "m"; var_value = VInt (2); is_const = false;
     assignment_count = 1; visibility_level = 1 }
  
  "c" ->
   { var_type = Int; var_key = "c"; var_value = VInt (3); is_const = false;
     assignment_count = 7; visibility_level = 0 }
  
  "i" ->
   { var_type = Int; var_key = "i"; var_value = VInt (3); is_const = false;
     assignment_count = 4; visibility_level = 0 }
  
  ]]
  ; current_method_type = Void; last_expr_result = VInt (3);
  runtime_signal = NoSignal; count_of_nested_cycles = 0; visibility_level = 1
  }
  
  --- Break test ---
  
  0
  1
  { variable_table = [[]]
    ; current_method_type = Void; last_expr_result = VInt (2);
    runtime_signal = NoSignal; count_of_nested_cycles = 0; visibility_level = 1
    }
  
  --- Continue test ---
  
  0
  1
  3
  { variable_table = [[]]
    ; current_method_type = Void; last_expr_result = VBool (false);
    runtime_signal = NoSignal; count_of_nested_cycles = 0; visibility_level = 1
    }
  
  --- Recursion test ---
  
  { variable_table =
    [["res" ->
       { var_type = Int; var_key = "res"; var_value = VInt (120);
         is_const = false; assignment_count = 1; visibility_level = 0 }
    
  ]]
  ; current_method_type = Void; last_expr_result = VInt (120);
  runtime_signal = NoSignal; count_of_nested_cycles = 0; visibility_level = 0
  }
  
  --- Const variables test ---
  
  Assigment to a constant variable
