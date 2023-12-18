namespace tansaku.Strategy

open tansaku.BoardGame
open tansaku.BoardGame.Action
open tansaku.BoardGame.StateValues

type BeamSearch(depth: int, width: int) =
    interface IStrategy with
        member this.Name = "ビームサーチ"
        member this.SelectAction(state: State): IMoveAction =
            let rec search (currentStates: (State * IMoveAction list) list) (depth: int): (State * IMoveAction list) list =
                match depth with
                | 0 -> currentStates
                | _ ->
                    currentStates
                    |> List.collect (fun (st, actions) ->
                        st.EnableActions() |> List.map (fun next -> (State(st.NextState(next)), actions @ [next])))
                    |> List.sortByDescending (fun (st, _) -> st.Score.Value)
                    |> List.take width
                    |> fun nextStates -> search nextStates (depth - 1)
            
            let initialStates = [(state, [])]
            let resultStates = search initialStates depth
            
            resultStates
            |> List.maxBy (fun (st, _) -> st.Score.Value)
            |> snd
            |> List.head
    
    member private this.NextStates(state: State): (IMoveAction * StateValue) list =
        state.EnableActions()
            |> List.map (fun action -> (action, state.NextState(action)))
            |> List.sortByDescending (fun (_, nextState) -> nextState.Score)
    