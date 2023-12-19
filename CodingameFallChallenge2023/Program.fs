open System

let tap f x = f x; x

type CreatureInfo = {
    Id : int
    Color : int
    Type : int
}

type Game = {
    Creatures: CreatureInfo list
}

let initialize: Game = 
    let creatureCount = int(Console.In.ReadLine())
    let creatures =
        [0 .. creatureCount - 1]
        |> List.map (fun _ -> Console.In.ReadLine())
        |> List.map (fun line -> line.Split [|' '|])
        |> List.map (fun token -> {
            Id = int(token[0])
            Color = int(token[1])
            Type = int(token[2])
        })

    {
        Creatures = creatures
    }

type Creature = {
    Id: int
    X: int
    Y: int
    Vx: int
    Vy: int
}

type DroneScan = {
    DroneId: int
    CreatureId: int
}

type Drone = {
    Id: int
    X: int
    Y: int
    Emergency: int
    Battery: int
}

type RadarBlip = {
    DroneId: int
    CreatureId: int
    Radar: string
}

type GameData = {
    MyScore: int
    FoeScore: int
    MyScanCount: int list
    FoeScanCount: int list
    MyDrones: Drone list
    FoeDrones: Drone list
    DroneScans: DroneScan list
    VisibleCreatures: Creature list
    RadarBlips: RadarBlip list
}

type IAction =
    abstract member ToString: unit -> string

type Reader() =
    let readInt() = Console.In.ReadLine() |> int

    let readDroneScans count =
        [0 .. count - 1]
        |> List.map (fun _ -> Console.In.ReadLine())
        |> List.map (fun line -> line.Split [|' '|])
        |> List.map (fun token -> {
            DroneId = int(token[0])
            CreatureId = int(token[1])
        })

    let readCreatures count =
        [0 .. count - 1]
        |> List.map (fun _ -> Console.In.ReadLine())
        |> List.map (fun line -> line.Split [|' '|])
        |> List.map (fun token -> {
            Id = int(token[0])
            X = int(token[1])
            Y = int(token[2])
            Vx = int(token[3])
            Vy = int(token[4])
        })

    let readDrones count =
        [0 .. count - 1]
        |> List.map (fun _ -> Console.In.ReadLine())
        |> List.map (fun line -> line.Split [|' '|])
        |> List.map (fun token -> {
            Id = int(token[0])
            X = int(token[1])
            Y = int(token[2])
            Emergency = int(token[3])
            Battery = int(token[4])
        })

    let readRadars count = 
        [0 .. count - 1]
        |> List.map (fun _ -> Console.In.ReadLine())
        |> List.map (fun line -> line.Split [|' '|])
        |> List.map (fun token -> {
            DroneId = int(token[0])
            CreatureId = int(token[1])
            Radar = token[2]
        })

    let readGameData(): GameData = {
        MyScore = readInt()
        FoeScore = readInt()
        MyScanCount = [0 .. readInt() - 1] |> List.map (fun _ -> readInt())
        FoeScanCount = [0 .. readInt() - 1] |> List.map (fun _ -> readInt())
        MyDrones = readDrones (readInt())
        FoeDrones = readDrones (readInt())
        DroneScans = readDroneScans (readInt()) 
        VisibleCreatures = readCreatures (readInt())
        RadarBlips = readRadars (readInt()) 
    }

    member this.ReadGameData() = readGameData()

type Writer() =
    member this.WriteAction(action: IAction) = Console.Out.WriteLine(action.ToString())

type Wait(light: int) =
    interface IAction with
        member this.ToString() = $"WAIT {light}"

type Move(x: int, y: int, light: int) =
    interface IAction with
        member this.ToString() = $"MOVE {x} {y} {light}"

type IStrategy =
    abstract member NextAction: GameData -> IAction

type Drifter(light: int) =
    interface IStrategy with
        member this.NextAction _ = Wait(light)
  
let game = initialize
let reader = Reader()
let writer = Writer()
let strategy: IStrategy = Drifter(0)

while true do
    let gameData = reader.ReadGameData()
    stderr.WriteLine $"GameData: %A{gameData}" 
    
    strategy.NextAction(gameData) |> writer.WriteAction

    ()
