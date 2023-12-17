namespace tansaku.BoardGame

type Board =
    { Value : int[,] }
    member this.IsInBounds(coordinate: Coordinate) =
        let rows = this.Value.GetLength(0)
        let cols = this.Value.GetLength(1)
        coordinate.X >= 0 && coordinate.X < cols &&
        coordinate.Y >= 0 && coordinate.Y < rows
    
    member this.Get(coordinate: Coordinate) =
        if this.IsInBounds(coordinate) then
            this.Value.[coordinate.Y, coordinate.X]
        else
            raise (new System.ArgumentOutOfRangeException("coordinate"))
    
    member this.Set(coordinate: Coordinate, value: int): Board =
        if this.IsInBounds(coordinate) then
            let newValue = this.Value.Clone() :?> int[,]
            newValue.[coordinate.Y, coordinate.X] <- value
            { Value = newValue }
        else
            raise (new System.ArgumentOutOfRangeException("coordinate"))
