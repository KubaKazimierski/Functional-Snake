module GameState

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Maybe

let TimeStep = 200.0

type Direction = Up | Down | Right | Left
type BoardCell = SnakeSegment of Direction | Fruit | Empty

type GameBoard = {  cells: BoardCell List;
                    size: Vector2;
                 }

type GameState = { board: GameBoard;
                   head: Vector2;
                   tail: Vector2;
                   grow: bool;
                   close: bool;
                   delta: float;
                 }

let defaultBoard x y = { cells = [for i in [1..x*y] -> Empty] |>
                                 fun l -> SnakeSegment Down :: List.tail l;
                         size = new Vector2(float32(x), float32(y))
                       }

let defaultGameState = { board = defaultBoard 20 20;
                         head = Vector2.Zero;
                         tail = Vector2.Zero;
                         grow = false;
                         close = false;
                         delta = 0.0 
                       }

let directionVector d = 
    match d with
        | Up    -> - Vector2.UnitY
        | Down  ->   Vector2.UnitY
        | Right ->   Vector2.UnitX
        | Left  -> - Vector2.UnitX

let directionInverse d =
    match d with
    | Up    -> Down
    | Down  -> Up
    | Right -> Left
    | Left  -> Right

let boardWithChangedCell board x y nv = 
    let first = List.take (x + y * int(board.size.Y)) board.cells in
        let last  = ListUtils.drop (x + y * int(board.size.Y) + 1) board.cells in
            { board with cells = first @ (nv :: last) }

let cellValue board (vec: Vector2) =
    board.cells.[int(vec.X) + int(vec.Y) * (int(board.size.Y))]

let rec boardWithNewFruit board (rng : System.Random) =
    let pos = new Vector2(float32(rng.Next(int(board.size.X))), float32(rng.Next(int(board.size.X)))) in
        if cellValue board pos <> Empty then
            boardWithNewFruit board rng
        else
            boardWithChangedCell board (int(pos.X)) (int(pos.Y)) Fruit

let canMove board (pos: Vector2) = 
    let rect = new Rectangle(Vector2.Zero.ToPoint(), board.size.ToPoint()) in
        rect.Contains(pos) &&
        match cellValue board pos with
        | SnakeSegment _ -> false
        | _              -> true

let updateHead state = 
    let updateHead' pos v = 
        { state with 
            head  = pos;
            board = boardWithChangedCell state.board (int(pos.X)) (int(pos.Y)) v;
            grow  = state.grow || cellValue state.board pos = Fruit;
        }

    let chead = (cellValue state.board state.head) in
        match chead with
        | SnakeSegment v ->
            let h = state.head + (directionVector v) in
                if canMove state.board h then
                    Just (updateHead' h chead)
                else
                    None
        | _ -> None

let updateTail state = 
    let updateTail' t =
         { state with 
            tail  = t;
            board = boardWithChangedCell state.board (int(state.tail.X)) (int(state.tail.Y)) Empty;
            grow  = false;
         }

    let ctail = (cellValue state.board state.tail) in
    match ctail with
    | SnakeSegment v ->
        if not state.grow then
            let t = state.tail + (directionVector v) in
                Just (updateTail' t)
        else
            Just state
    | _  -> None

let updateBoard rng state = 
    if state.grow then 
        Just { state with board = boardWithNewFruit state.board rng }
    else 
        Just state

let updatedState rng state =
    if state.delta > TimeStep then
        match (updateHead state >>= updateBoard rng >>= updateTail) with
        | Just s -> { s with delta = 0.0; grow = false }
        | None   -> { state with close = true } 
    else
        state

let mappedDirection (key: Keys) = 
    match key with
    | Keys.Up    -> Just Up
    | Keys.Down  -> Just Down
    | Keys.Right -> Just Right
    | Keys.Left  -> Just Left
    | _          -> None

let isDirectionCorrect state d =
    match (cellValue state.board state.head) with
    | SnakeSegment v -> canMove state.board (state.head + (directionVector d))
    | _   -> false

let handleInput state (keyboardState: KeyboardState) =
    let newBoard =
        List.map mappedDirection (Array.toList (keyboardState.GetPressedKeys())) |>
        List.fold (fun acc i -> if acc = None then i else acc) None >>= 
        fun r -> 
            if isDirectionCorrect state r then
                Just (boardWithChangedCell state.board 
                    (int(state.head.X)) (int(state.head.Y)) (SnakeSegment r))
            else None
    in

    match newBoard with
    | Just v -> { state with board = v }
    | None   -> state