namespace tansaku.Board

type Game() =
    static member val Height = 3
    static member val Width = 4
    static member val EndTurn = 4
    
    member this.Play() =
        this.InitialTurn()
        for turn in 1..Game.EndTurn do
            this.PlayTurn()
            
        ()
        
    member private this.InitialTurn() =
        printfn "initial turn"
    
    member private this.PlayTurn() =
        printfn "play turn"
