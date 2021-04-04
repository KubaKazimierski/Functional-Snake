//Author: Jakub Kazimierski

[<EntryPoint>]
let main argv =
    use game = new SnakeGame.SnakeGame()
    game.Run()
    0
