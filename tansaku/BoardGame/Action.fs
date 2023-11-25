namespace tansaku.BoardGame

module Action =
    let MoveNorth (coordinate:Coordinate): Coordinate =
        { X = coordinate.X; Y = coordinate.Y - 1 }
    
    let MoveSouth (coordinate:Coordinate): Coordinate =
        { X = coordinate.X; Y = coordinate.Y + 1 }
        
    let MoveEast (coordinate:Coordinate): Coordinate =
        { X = coordinate.X + 1; Y = coordinate.Y }

    let MoveWest (coordinate:Coordinate): Coordinate =
            { X = coordinate.X - 1; Y = coordinate.Y }
    
    type MoveActions = MoveNorth | MoveSouth | MoveEast | MoveWest
    
    let ActionName(action: MoveActions option): string =
        match action with
        | Some validAction ->
            match validAction with
            | MoveEast _ -> "MoveEast"
            | MoveNorth _ -> "MoveNorth"
            | MoveSouth _ -> "MoveSouth"
            | MoveWest _ -> "MoveWest"
        | None _ -> "None"
