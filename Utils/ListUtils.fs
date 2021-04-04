module ListUtils

let drop n l =
    List.rev l |> List.take (l.Length - n) |> List.rev