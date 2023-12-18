namespace tansaku.BoardGame

module Turns =
    type Turn = { Value : int }

    let (+) (a: Turn) (b: Turn): Turn =
        { Value = a.Value + b.Value }
    
    let (-) (a: Turn) (b: Turn): Turn =
        { Value = a.Value - b.Value }
