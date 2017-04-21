module Option = struct
  let bind f x =
    match x with
    | Some x -> f x
    | None -> None

  let return x = Some x

  let map f = function
    | Some x -> Some (f x)
    | None -> None
end
