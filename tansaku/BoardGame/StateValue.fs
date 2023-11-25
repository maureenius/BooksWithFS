namespace tansaku.BoardGame

open tansaku.BoardGame.Action

type StateValue = {
    Score : Score
    Turn : Turn
    Board : Board
    Character : Coordinate
    BeforeAction : MoveActions option
}