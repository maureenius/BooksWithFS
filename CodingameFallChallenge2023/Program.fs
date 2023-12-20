open System

let tap f x = f x; x

module GameData =
    type CreatureInfo = {
        Id : int
        Color : int
        Type : int
    }

    type Game = {
        Creatures: CreatureInfo list
    }

    type Coordinate =
        {
            X: int
            Y: int
        }
        member this.DistanceTo(other: Coordinate) =
            let dx = this.X - other.X
            let dy = this.Y - other.Y
            dx * dx + dy * dy

    type Creature = {
        Id: int
        Coordinate: Coordinate
        Vx: int
        Vy: int
    }

    type DroneScan = {
        DroneId: int
        CreatureId: int
    }

    type Drone = {
        Id: int
        Coordinate: Coordinate
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
        CurrentTurn: int
        MaxTurn: int
    }

module Actions =
    type IAction =
        abstract member ToString: unit -> string

    type Commands = {
        Drone1Action: IAction
        Drone2Action: IAction
    }
    
    type Wait(light: int) =
        interface IAction with
            member this.ToString() = $"WAIT {light}"

    type Move(coordinate: GameData.Coordinate, light: int) =
        interface IAction with
            member this.ToString() = $"MOVE {coordinate.X} {coordinate.Y} {light}"

module IOHelper =
    let readInt() = Console.In.ReadLine() |> int
    
    let initialize: GameData.Game = 
        let creatureCount = int(Console.In.ReadLine())
        let creatures =
            [0 .. creatureCount - 1]
            |> List.map (fun _ -> Console.In.ReadLine())
            |> List.map (fun line -> line.Split [|' '|])
            |> List.map (fun token -> {
                Id = int(token[0])
                Color = int(token[1])
                Type = int(token[2])
            }): GameData.CreatureInfo list

        {
            Creatures = creatures
        }

    let readDroneScans (count: int): GameData.DroneScan list =
        [0 .. count - 1]
        |> List.map (fun _ -> Console.In.ReadLine())
        |> List.map (fun line -> line.Split [|' '|])
        |> List.map (fun token -> { DroneId = int(token[0]); CreatureId = int(token[1]) })

    let readCreatures (count: int): GameData.Creature list =
        [0 .. count - 1]
        |> List.map (fun _ -> Console.In.ReadLine())
        |> List.map (fun line -> line.Split [|' '|])
        |> List.map (fun token -> {
            Id = int(token[0])
            Coordinate = { X = int(token[1]); Y = int(token[2])}
            Vx = int(token[3])
            Vy = int(token[4])
        })

    let readDrones (count: int): GameData.Drone list =
        [0 .. count - 1]
        |> List.map (fun _ -> Console.In.ReadLine())
        |> List.map (fun line -> line.Split [|' '|])
        |> List.map (fun token -> {
            Id = int(token[0])
            Coordinate = { X = int(token[1]); Y = int(token[2])}
            Emergency = int(token[3])
            Battery = int(token[4])
        })

    let readRadars (count: int): GameData.RadarBlip list = 
        [0 .. count - 1]
        |> List.map (fun _ -> Console.In.ReadLine())
        |> List.map (fun line -> line.Split [|' '|])
        |> List.map (fun token -> {
            DroneId = int(token[0])
            CreatureId = int(token[1])
            Radar = token[2]
        })

    let readGameData (maxTurn: int) (currentTurn: int): GameData.GameData = {
        MyScore = readInt()
        FoeScore = readInt()
        MyScanCount = [0 .. readInt() - 1] |> List.map (fun _ -> readInt())
        FoeScanCount = [0 .. readInt() - 1] |> List.map (fun _ -> readInt())
        MyDrones = readDrones (readInt())
        FoeDrones = readDrones (readInt())
        DroneScans = readDroneScans (readInt()) 
        VisibleCreatures = readCreatures (readInt())
        RadarBlips = readRadars (readInt())
        CurrentTurn = currentTurn
        MaxTurn = maxTurn
    }

    let writeAction (commands: Actions.Commands) =
        Console.Out.WriteLine(commands.Drone1Action.ToString())
        Console.Out.WriteLine(commands.Drone2Action.ToString())

module GameLogic =
    let surface = 500
    let droneSpeed = 600
    
    let shouldReturnToSurface (gameData: GameData.GameData) (drone: GameData.Drone) =
        let distanceToSurface = drone.Coordinate.Y - surface 
        let turnsToSurface = distanceToSurface / droneSpeed   // 水面に戻るのに必要なターン数
        let buffer = 10
        let remainingTurns = gameData.MaxTurn - gameData.CurrentTurn // 残りターン数
        turnsToSurface >= remainingTurns - buffer

module Strategies =
    type IStrategy =
        abstract member NextActions: GameData.GameData -> Actions.Commands

    type Nearest() =
        let nearestFish (gameData: GameData.GameData) =
            gameData.VisibleCreatures
            |> List.filter (fun creature -> not (List.contains creature.Id gameData.MyScanCount))
            |> List.minBy (fun creature -> creature.Coordinate.DistanceTo(gameData.MyDrones.Head.Coordinate))

        let isScanned (gameData: GameData.GameData) creatureId =
            (List.contains creatureId gameData.MyScanCount) ||
            (
                gameData.DroneScans
                |> List.filter (fun scan -> scan.DroneId = gameData.MyDrones.Head.Id)
                |> List.exists (fun scan -> scan.CreatureId = creatureId)
            )
            
        let hasUnscannedFish (gameData: GameData.GameData) =
            gameData.DroneScans
            |> List.filter (fun scan -> scan.DroneId = gameData.MyDrones.Head.Id)
            |> List.filter (fun scan -> not (List.contains scan.CreatureId gameData.FoeScanCount))
            |> fun scans -> scans.Length > 0
        
        interface IStrategy with
            member this.NextActions gameData =
                let drone = gameData.MyDrones.Head |> tap (fun drone -> stderr.WriteLine $"drone: {drone}")
                
                let radarInfo =
                    gameData.RadarBlips
                    |> List.filter (fun blip -> not (isScanned gameData blip.CreatureId))
                    |> List.tryHead
                    |> tap (fun blip -> stderr.WriteLine $"blip: {blip}")

                if GameLogic.shouldReturnToSurface gameData drone || hasUnscannedFish gameData then
                    {
                        Drone1Action = Actions.Move({ X = drone.Coordinate.X; Y = GameLogic.surface }, 0)
                        Drone2Action = Actions.Wait(0)
                    }
                else
                    let nextCoordinate: GameData.Coordinate option =
                        match radarInfo with
                        | Some blip ->
                            match blip.Radar with
                            | "TL" -> Some { X = drone.Coordinate.X - GameLogic.droneSpeed; Y = drone.Coordinate.Y - GameLogic.droneSpeed }
                            | "TR" -> Some { X = drone.Coordinate.X + GameLogic.droneSpeed; Y = drone.Coordinate.Y - GameLogic.droneSpeed }
                            | "BR" -> Some { X = drone.Coordinate.X + GameLogic.droneSpeed; Y = drone.Coordinate.Y + GameLogic.droneSpeed }
                            | "BL" -> Some { X = drone.Coordinate.X - GameLogic.droneSpeed; Y = drone.Coordinate.Y + GameLogic.droneSpeed }
                            | _    -> stderr.WriteLine "レーダー情報の読み込みに失敗"; None
                        | None -> None
                    stderr.WriteLine $"nextCoordinate: {nextCoordinate}"    
                    
                    match nextCoordinate with
                    | Some coordinate -> { Drone1Action = Actions.Move(coordinate, 0); Drone2Action = Actions.Wait(0) }
                    | None -> { Drone1Action = Actions.Wait(0); Drone2Action = Actions.Wait(0) }

module Main =
    let gamePlay() =
        let MaxTurn = 200
        let _ = IOHelper.initialize
        let strategy: Strategies.IStrategy = Strategies.Nearest()

        [1 .. MaxTurn]
        |> List.iter (fun turn -> 
            let gameData = IOHelper.readGameData MaxTurn turn
            strategy.NextActions(gameData) |> IOHelper.writeAction
        )

Main.gamePlay()
