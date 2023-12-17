namespace tansaku.Strategy

open tansaku.BoardGame
open tansaku.BoardGame.Action

type Random() =
    interface IStrategy with
        member this.SelectAction(state: State): IMoveAction =
            state.EnableActions()
            |> Random.Choose

        member this.Name = "ランダム選択"

    static member private Choose<'T>(list: 'T list): 'T =
        let rnd = System.Random()
        let index = rnd.Next(List.length list)
        List.item index list
