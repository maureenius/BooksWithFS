namespace tansaku

open tansaku.BoardGame
open tansaku.BoardGame.Scores
open tansaku.BoardGame.StateValues
open tansaku.BoardGame.Turns
open tansaku.Strategy

type Game(strategy: IStrategy) =
    let initialCharacter: Coordinate =
        new System.Random()
        |> fun rnd -> { H = rnd.Next(Game.Height.Value); W = rnd.Next(Game.Width.Value) }
    
    let mutable state: StateValue =
        {
            Score = { Value = 0 }
            Turn = { Value = 0 }
            Board =
                { Value = Array2D.init Game.Height.Value Game.Width.Value (Game.CreateBoardValue initialCharacter) }
            Character = initialCharacter
            BeforeAction = None 
        }
    
    static member val Height: Height = { Value = 3 }
    static member val Width: Width = { Value = 4 }
    static member val EndTurn: Turn = { Value = 4 }
    
    static member val ScoreMax: Score = { Value = 9 }
    
    member this.Play() =
        this.StartGame()

        for turn in 1..Game.EndTurn.Value do
            this.View()
            state <- this.PlayTurn(State(state), strategy)
            
        this.FinishGame()
    
    static member private CreateBoardValue (zero: Coordinate) (h: int) (w: int): int =
        if zero.H = h && zero.W = w then
            0
        else
            System.Random() |> fun rnd -> rnd.Next(Game.ScoreMax.Value)
    
    member private this.View() =
        stdout.WriteLine (State(state).View() |> String.concat "\n")
        
    member private this.StartGame() =
        stdout.WriteLine("Game Start!")
        
    member private this.FinishGame() =
        stdout.WriteLine("Result")
        this.View()
        
        stdout.WriteLine("Game Finish!")
    
    member private this.PlayTurn(state: State, strategy: IStrategy): StateValue =
        strategy.SelectAction(state)
        |> state.NextState
