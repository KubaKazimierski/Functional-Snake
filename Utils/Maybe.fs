module Maybe

type Maybe<'a> =
    | Just of 'a
    | None

let bind m n =
    match m with
    | Just v -> n v
    | None   -> None

let (>>=) = bind
let (>>) m n = m >>= fun _ -> n

let ret a =
    Just a
    
