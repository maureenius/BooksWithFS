namespace tansaku.Strategy

open tansaku.BoardGame
open tansaku.BoardGame.Action

type IStrategy =
    abstract member SelectAction: State -> IMoveAction
