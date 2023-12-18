namespace tansaku.BoardGame

open tansaku.BoardGame.Action
open tansaku.BoardGame.StateValues
open tansaku.BoardGame.Turns
open tansaku.BoardGame.Scores

type State(value: StateValue) =
    member this.NextState(action: IMoveAction): StateValue =
        {
            Score = value.Score
            Turn = Turns.(+) value.Turn { Value = 1 }
            Board = value.Board
            Character = value.Character
            BeforeAction = value.BeforeAction
        }
        |> fun state -> this.Move(action, state)
    
    member this.Score = value.Score
    
    member this.View(): string list =
        [this.ActionView(); this.TurnView(); this.ScoreView(); this.BoardView()]
    
    member this.EnableActions(): IMoveAction list =
        AllMoveActions
        |> List.filter (fun action -> this.CanMove(value.Character, action))

    member private this.ActionView() =
        $"""action: %s{value.BeforeAction |> Option.map (fun action -> action.Name) |> Option.defaultValue "None" }"""

    member private this.TurnView() =
        $"turn: %s{value.Turn.Value.ToString()}"
        
    member private this.ScoreView() =
        $"score: %s{value.Score.Value.ToString()}"
    
    member private this.BoardView() =
        let height, width = Array2D.length1 value.Board.Value, Array2D.length2 value.Board.Value

        [ for h in 0 .. height - 1 do
            let row =
                [ for w in 0 .. width - 1 do
                    let coordinate: Coordinate = { H = h; W = w }
                    if this.ExistCharacter(coordinate) then "@"
                    elif value.Board.Value[h, w] = 0 then "."
                    else value.Board.Value[h, w].ToString() ]
                |> String.concat ""
            yield row ]
        |> String.concat "\n"
    
    member private this.ExistCharacter(coordinate: Coordinate): bool =
        value.Character = coordinate

    member private this.CanMove(character: Coordinate, action: IMoveAction): bool =
        action.Execute character
        |> value.Board.IsInBounds

    member private this.Move (action: IMoveAction, state: StateValue): StateValue =
        action.Execute state.Character
        |> fun coordinate -> { state with Character = coordinate }
        |> fun state -> { state with Score = state.Score + { Value = state.Board.Value.[state.Character.H, state.Character.W] } }
        |> fun state -> { state with Board = state.Board.Set(state.Character, 0) }
        |> fun state -> { state with BeforeAction = Some action }
