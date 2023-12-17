namespace tansaku.BoardGame

module Action =
    type IMoveAction =
        abstract member Execute: Coordinate -> Coordinate
        abstract member Name: string

    type MoveNorth() =
        interface IMoveAction with
            member this.Execute coordinate = { X = coordinate.X; Y = coordinate.Y - 1 }
            member this.Name = "MoveNorth"
    
    type MoveSouth() =
        interface IMoveAction with
            member this.Execute coordinate = { X = coordinate.X; Y = coordinate.Y + 1 }
            member this.Name = "MoveSouth"
    
    type MoveEast() =
        interface IMoveAction with
            member this.Execute coordinate = { X = coordinate.X + 1; Y = coordinate.Y }
            member this.Name = "MoveEast"

    type MoveWest() =
        interface IMoveAction with
            member this.Execute coordinate = { X = coordinate.X - 1; Y = coordinate.Y }
            member this.Name = "MoveWest"

    let AllMoveActions: IMoveAction list = [ MoveNorth(); MoveSouth(); MoveEast(); MoveWest() ]
