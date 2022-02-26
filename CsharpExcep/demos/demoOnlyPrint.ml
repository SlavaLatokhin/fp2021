open Csharp_lib.Test_interpreter

let s = Stdio.In_channel.input_all stdin
let () = interpret s false
