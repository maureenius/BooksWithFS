namespace tansaku.BoardGame

type Game() =
    let mutable state: StateValue = {
            Score = { Value = 0 }
            Turn = { Value = 1 }
            Board = { Value = Array2D.init Game.Height.Value Game.Width.Value (fun _ _ -> 2) }
            Character = { X = 1; Y = 1 }
            BeforeAction = None 
        }
    
    static member val Height: Height = { Value = 3 }
    static member val Width: Width = { Value = 4 }
    static member val EndTurn: Turn = { Value = 4 }
    
    member this.Play() =
        this.InitialTurn()

        for turn in 1..Game.EndTurn.Value do
            this.PlayTurn()
            
        ()
        
    member private this.InitialTurn() =
        stdout.WriteLine (State(state).View() |> String.concat "\n")
        printfn "initial turn"
    
    member private this.InitialBoardValue(x, y) = 0
    
    member private this.PlayTurn() =
        printfn "play turn"
