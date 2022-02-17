type ('a, 'b) t = ('a, 'b) Hashtbl.t

let pp pp_key pp_value ppf ht =
  Format.fprintf ppf "[["
  |> fun () ->
  Hashtbl.iter
    (fun key data ->
      Format.fprintf ppf "@[<1>%a@ ->@ %a@]@\n@." pp_key key pp_value data )
    ht
  |> fun () -> Format.fprintf ppf "]]@\n"
