namespace tansaku.BoardGame

module Scores =

    type Score = { Value : int }

    let (+) (a: Score) (b: Score): Score =
        { Value = a.Value + b.Value }
