namespace tansaku.BoardGame

open tansaku.BoardGame.Action
open tansaku.BoardGame.Scores
open tansaku.BoardGame.Turns

module StateValues =
    type StateValue = {
        Score : Score
        Turn : Turn
        EndTurn : Turn
        Board : Board
        Character : Coordinate
        BeforeAction : IMoveAction option
    }
