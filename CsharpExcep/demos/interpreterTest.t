  $ (cd ../../../../default && demos/demoInterpreter.exe) 
  --- Assign test ----
  
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
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
  runtime_signal = NoSignal; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 0; prev_ctx = None;
  count_of_obj = 0; is_creation = false
  }
  
  --- Arithmetic test ---
  
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
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
  runtime_signal = NoSignal; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 0; prev_ctx = None;
  count_of_obj = 0; is_creation = false
  }
  
  --- Boolean expression test ---
  
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["v11" ->
       { var_type = Int; var_key = "v11"; var_value = VInt (1);
         is_const = false; assignment_count = 1; visibility_level = 0 }
    
  "v12" ->
   { var_type = Int; var_key = "v12"; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "a" ->
   { var_type = Int; var_key = "a"; var_value = VInt (1); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "v1" ->
   { var_type = Int; var_key = "v1"; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "b" ->
   { var_type = Int; var_key = "b"; var_value = VInt (50); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  "v13" ->
   { var_type = Int; var_key = "v13"; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "p3" ->
   { var_type = CsClass ("Person"); var_key = "p3";
     var_value =
     VClass (ObjRef ({ class_key = "Person"; parent_key = None;
                       class_table =
                       [["age" ->
                          { key = "age"; field_type = Int;
                            field_value = VInt (20); is_const = false;
                            assignment_count = 0 }
                       
  "name" ->
   { key = "name"; field_type = String; field_value = VString ("Bob");
     is_const = false; assignment_count = 0 }
  
  ]]
  ; number = 1 })); is_const = false; assignment_count = 1;
  visibility_level = 0
  }
  
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
  
  "p2" ->
   { var_type = CsClass ("Person"); var_key = "p2";
     var_value =
     VClass (ObjRef ({ class_key = "Person"; parent_key = None;
                       class_table =
                       [["age" ->
                          { key = "age"; field_type = Int;
                            field_value = VInt (30); is_const = false;
                            assignment_count = 0 }
                       
  "name" ->
   { key = "name"; field_type = String; field_value = VString ("Alice");
     is_const = false; assignment_count = 0 }
  
  ]]
  ; number = 2 })); is_const = false; assignment_count = 1;
  visibility_level = 0
  }
  
  "v9" ->
   { var_type = Int; var_key = "v9"; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "v8" ->
   { var_type = Int; var_key = "v8"; var_value = VInt (1); is_const = false;
     assignment_count = 2; visibility_level = 0 }
  
  "p1" ->
   { var_type = CsClass ("Person"); var_key = "p1";
     var_value =
     VClass (ObjRef ({ class_key = "Person"; parent_key = None;
                       class_table =
                       [["age" ->
                          { key = "age"; field_type = Int;
                            field_value = VInt (20); is_const = false;
                            assignment_count = 0 }
                       
  "name" ->
   { key = "name"; field_type = String; field_value = VString ("Bob");
     is_const = false; assignment_count = 0 }
  
  ]]
  ; number = 1 })); is_const = false; assignment_count = 1;
  visibility_level = 0
  }
  
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
  runtime_signal = NoSignal; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 0; prev_ctx = None;
  count_of_obj = 2; is_creation = false
  }
  
  --- Cycles test ---
  
  0
  0
  0
  1
  0
  2
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["i" ->
       { var_type = Int; var_key = "i"; var_value = VInt (3); is_const = false;
         assignment_count = 4; visibility_level = 1 }
    
  ]]
  ; current_method_type = Void; last_expr_result = VBool (false);
  runtime_signal = NoSignal; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 2; prev_ctx = None;
  count_of_obj = 0; is_creation = false
  }
  
  --- While test ---
  
  4
  3
  2
  1
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["i" ->
       { var_type = Int; var_key = "i"; var_value = VInt (0); is_const = false;
         assignment_count = 5; visibility_level = 0 }
    
  ]]
  ; current_method_type = Void; last_expr_result = VBool (false);
  runtime_signal = NoSignal; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 0; prev_ctx = None;
  count_of_obj = 0; is_creation = false
  }
  
  --- If test ---
  
  a
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["a" ->
       { var_type = Int; var_key = "a"; var_value = VInt (8); is_const = false;
         assignment_count = 1; visibility_level = 0 }
    
  "b" ->
   { var_type = Int; var_key = "b"; var_value = VInt (6); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  ]]
  ; current_method_type = Void; last_expr_result = VString ("a");
  runtime_signal = NoSignal; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 0; prev_ctx = None;
  count_of_obj = 0; is_creation = false
  }
  
  --- If else if test ---
  
  a==b
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["a" ->
       { var_type = Int; var_key = "a"; var_value = VInt (6); is_const = false;
         assignment_count = 1; visibility_level = 0 }
    
  "b" ->
   { var_type = Int; var_key = "b"; var_value = VInt (6); is_const = false;
     assignment_count = 1; visibility_level = 0 }
  
  ]]
  ; current_method_type = Void; last_expr_result = VString ("a==b");
  runtime_signal = NoSignal; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 0; prev_ctx = None;
  count_of_obj = 0; is_creation = false
  }
  
  --- Visibility level test ---
  
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["a" ->
       { var_type = Int; var_key = "a"; var_value = VInt (1); is_const = false;
         assignment_count = 11; visibility_level = 0 }
    
  "b" ->
   { var_type = Int; var_key = "b"; var_value = VInt (2); is_const = false;
     assignment_count = 4; visibility_level = 0 }
  
  "c" ->
   { var_type = Int; var_key = "c"; var_value = VInt (3); is_const = false;
     assignment_count = 7; visibility_level = 0 }
  
  "i" ->
   { var_type = Int; var_key = "i"; var_value = VInt (3); is_const = false;
     assignment_count = 4; visibility_level = 0 }
  
  ]]
  ; current_method_type = Void; last_expr_result = VInt (3);
  runtime_signal = NoSignal; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 1; prev_ctx = None;
  count_of_obj = 0; is_creation = false
  }
  
  --- Break test ---
  
  0
  1
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table = [[]]
    ; current_method_type = Void; last_expr_result = VInt (3);
    runtime_signal = NoSignal; is_main = true; curr_constructor = None;
    count_of_nested_cycle = 0; visibility_level = 1; prev_ctx = None;
    count_of_obj = 0; is_creation = false }
  
  --- Continue test ---
  
  0
  1
  3
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table = [[]]
    ; current_method_type = Void; last_expr_result = VBool (false);
    runtime_signal = NoSignal; is_main = true; curr_constructor = None;
    count_of_nested_cycle = 0; visibility_level = 1; prev_ctx = None;
    count_of_obj = 0; is_creation = false }
  
  --- Recursion test ---
  
  { current_o =
    ObjRef ({ class_key = "Program"; parent_key = None; class_table = [[]]
                                                        ;
              number = 0 });
    variable_table =
    [["res" ->
       { var_type = Int; var_key = "res"; var_value = VInt (120);
         is_const = false; assignment_count = 1; visibility_level = 0 }
    
  ]]
  ; current_method_type = Void; last_expr_result = VInt (120);
  runtime_signal = NoSignal; is_main = true; curr_constructor = None;
  count_of_nested_cycle = 0; visibility_level = 0; prev_ctx = None;
  count_of_obj = 0; is_creation = false
  }
  
  --- Const variables test ---
  
  Assigment to a constant variable
