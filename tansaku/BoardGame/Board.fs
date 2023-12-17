namespace tansaku.BoardGame

type Board =
    { Value : int[,] }
    member this.IsInBounds(coordinate: Coordinate) =
        let rows = this.Value.GetLength(0)
        let cols = this.Value.GetLength(1)
        coordinate.H >= 0 && coordinate.H < rows &&
        coordinate.W >= 0 && coordinate.W < cols
    
    member this.Get(coordinate: Coordinate) =
        if this.IsInBounds(coordinate) then
            this.Value.[coordinate.H, coordinate.W]
        else
            raise (new System.ArgumentOutOfRangeException("coordinate"))
    
    member this.Set(coordinate: Coordinate, value: int): Board =
        if this.IsInBounds(coordinate) then
            let newValue = this.Value.Clone() :?> int[,]
            newValue.[coordinate.H, coordinate.W] <- value
            { Value = newValue }
        else
            raise (new System.ArgumentOutOfRangeException("coordinate"))
