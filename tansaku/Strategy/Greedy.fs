namespace tansaku.Strategy

open tansaku.BoardGame
open tansaku.BoardGame.Action

type Greedy() =
    interface IStrategy with
        member this.SelectAction(state: State): IMoveAction =
            state.EnableActions()
            |> List.map (fun action -> (action, state.NextState(action)))
            |> List.maxBy (fun (_, nextState) -> nextState.Score)
            |> fst
