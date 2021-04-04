module SnakeGame

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

let blankRectangle x y =
    [|for i in [1..x*y] -> Color.White|]

let CellSize = 15

type SnakeGame() as _this =
    inherit Game()

    let mutable graphics    = new GraphicsDeviceManager(_this)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let mutable state       = GameState.defaultGameState 
                              |> fun s -> { s with board = GameState.boardWithNewFruit s.board (new System.Random()) }
    let mutable recTexture  = Unchecked.defaultof<Texture2D>

    let drawRect(x, y, color) = 
        spriteBatch.Draw(recTexture, Vector2.Multiply(new Vector2(float32(x), float32(y)), float32(CellSize + 1)), color)
    
    member _this.baseUpdate(time) = base.Update(time)

    override _this.Initialize() =
        recTexture <- new Texture2D(_this.GraphicsDevice, CellSize, CellSize)
        recTexture.SetData(blankRectangle CellSize CellSize)
        base.Initialize()

    override _this.LoadContent() =
        spriteBatch <- new SpriteBatch(_this.GraphicsDevice)

    override _this.UnloadContent() = 
        ()

    override _this.Update(time) =
        let keyboarState = Keyboard.GetState()

        let stateWorkflow = async { 
            return
                { state with delta = state.delta + time.ElapsedGameTime.TotalMilliseconds } 
                |> GameState.updatedState (new System.Random())
                |> fun s -> GameState.handleInput s keyboarState 
        }

        if keyboarState.IsKeyDown(Keys.Escape) || state.close then
            _this.Exit()

        state <- 
            async { 
                let! child = Async.StartChild stateWorkflow
                _this.baseUpdate(time)
                
                let! result = child
                return result 
            } 
            |> Async.RunSynchronously
         

    override _this.Draw(time) =
        _this.GraphicsDevice.Clear Color.Black

        spriteBatch.Begin()

        for y in 0 .. (int(state.board.size.Y) - 1) do 
            for x in 0 .. (int(state.board.size.X) - 1) do
                 match (GameState.cellValue state.board (new Vector2(float32(x), float32(y)))) with
                 | GameState.SnakeSegment _ -> drawRect(x, y, Color.Red)
                 | GameState.Fruit -> drawRect(x, y, Color.Aqua)
                 | GameState.Empty -> drawRect(x, y, Color.Gray)

        spriteBatch.End()
        base.Draw(time)