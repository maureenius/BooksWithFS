namespace tansaku.BoardGame

type State(value: StateValue) =
    member this.View(): string list =
        [this.ActionView(); this.TurnView(); this.ScoreView(); this.BoardView()]

    member private this.ActionView() =
        $"action: %s{value.BeforeAction |> Action.ActionName}"
    
    member private this.TurnView() =
        $"turn: %s{value.Turn.Value.ToString()}"
        
    member private this.ScoreView() =
        $"score: %s{value.Score.Value.ToString()}"
    
    member private this.BoardView() =
        let height, width = Array2D.length1 value.Board.Value, Array2D.length2 value.Board.Value

        [ for y in 0 .. height - 1 do
            let row =
                [ for x in 0 .. width - 1 do
                    let coordinate: Coordinate = { X = x; Y = y }
                    if this.ExistCharacter(coordinate) then "@"
                    elif value.Board.Value[y, x] = 0 then "."
                    else value.Board.Value[y, x].ToString() ]
                |> String.concat ""
            yield row ]
        |> String.concat "\n"
    
    member private this.ExistCharacter(coordinate: Coordinate): bool =
        value.Character = coordinate
