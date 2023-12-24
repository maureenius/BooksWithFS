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

open GameData

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

open Actions

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
        Console.Out.WriteLine(commands.Drone1Action.ToString()) |> tap (fun _ -> stderr.WriteLine (commands.Drone1Action.ToString()))
        Console.Out.WriteLine(commands.Drone2Action.ToString()) |> tap (fun _ -> stderr.WriteLine (commands.Drone2Action.ToString()))

module GameLogic =
    type GameLogic(game: GameData.Game, gameData: GameData.GameData) =
        let surface = 500
        let droneSpeed = 600
        let height = 10000
        let width = 10000
        
        member this.Surface = surface
        member this.DroneSpeed = droneSpeed
        member this.Height = height
        member this.Width = width
        
        override this.ToString() =
            $"{game}\n{gameData}"
        
        member this.ShallowFishes =
            game.Creatures |> List.filter (fun creature -> creature.Type = 0)
        
        member this.IsShallowClear = this.ShallowFishes |> List.isEmpty
        
        member this.MiddleFishes =
            game.Creatures |> List.filter (fun creature -> creature.Type = 1)
            
        member this.IsMiddleClear = this.MiddleFishes |> List.isEmpty
            
        member this.DeepFishes =
            game.Creatures |> List.filter (fun creature -> creature.Type = 2)
        
        member this.IsDeepClear = this.DeepFishes |> List.isEmpty
        
        member this.CreatureRadars (creature: CreatureInfo) =
            gameData.RadarBlips
            |> List.filter (fun blip -> blip.CreatureId = creature.Id)
            
        member this.FindRadars (targets: CreatureInfo list): RadarBlip list =
            gameData.RadarBlips
            |> List.filter (fun blip -> List.contains blip.CreatureId (targets |> List.map (fun fish -> fish.Id)))
        
        member this.shouldLastReturnToSurface (drone: GameData.Drone) =
            let distanceToSurface = drone.Coordinate.Y - surface 
            let turnsToSurface = distanceToSurface / droneSpeed   // 水面に戻るのに必要なターン数
            let buffer = 10
            let remainingTurns = gameData.MaxTurn - gameData.CurrentTurn // 残りターン数
            turnsToSurface >= remainingTurns - buffer
        
        member this.nearestFish (drone: Drone) =
                gameData.VisibleCreatures
                |> List.filter (fun creature -> not (List.contains creature.Id gameData.MyScanCount))
                |> List.minBy (fun creature -> creature.Coordinate.DistanceTo(drone.Coordinate))

        member this.IsSearchableFish (fish: Creature): bool =
            fish.Coordinate.X >= 0 && fish.Coordinate.X <= width && fish.Coordinate.Y >= 0 && fish.Coordinate.Y <= height
        
        member this.IsSavedByMe (creature: CreatureInfo) =
            List.contains creature.Id gameData.MyScanCount
        
        member this.IsScannedAndUnsavedByMe (creature: CreatureInfo) =
            gameData.DroneScans
            |> List.filter (fun scan -> List.contains scan.DroneId (gameData.MyDrones |> List.map (fun drone -> drone.Id)))
            |> List.exists (fun scan -> scan.CreatureId = creature.Id)
        
        member this.IsScannedByMe (creature: CreatureInfo) =
            this.IsSavedByMe creature || this.IsScannedAndUnsavedByMe creature
        
        member this.UnknownFishes (targets: CreatureInfo list): CreatureInfo list =
            targets
            |> List.filter (fun creature -> this.IsScannedByMe creature |> not) 
        
        member this.UnSavedFishes (drone: Drone) =
                game.Creatures
                |> List.filter (fun creature -> List.contains creature.Id (gameData.DroneScans |> List.map (fun scan -> scan.CreatureId)))
                |> List.filter (fun creature -> not (List.contains creature.Id gameData.MyScanCount)) 
        
        member this.hasUnscannedFish drone =
                gameData.DroneScans
                |> List.filter (fun scan -> scan.DroneId = drone.Id)
                |> List.filter (fun scan -> not (List.contains scan.CreatureId gameData.FoeScanCount))
                |> fun scans -> scans.Length > 0
        
        member this.FollowRadar (radar: RadarBlip) (drone: Drone): Coordinate =
            match radar.Radar with
            | "TL" -> { X = drone.Coordinate.X - droneSpeed; Y = drone.Coordinate.Y - droneSpeed }
            | "TR" -> { X = drone.Coordinate.X + droneSpeed; Y = drone.Coordinate.Y - droneSpeed }
            | "BR" -> { X = drone.Coordinate.X + droneSpeed; Y = drone.Coordinate.Y + droneSpeed }
            | "BL" -> { X = drone.Coordinate.X - droneSpeed; Y = drone.Coordinate.Y + droneSpeed }
            | _    -> stderr.WriteLine "レーダー情報の読み込みに失敗"; drone.Coordinate

module Strategies =
    type IStrategy =
        abstract member NextActions: GameData.Game -> GameData.GameData -> Actions.Commands
    
    type DepthSplitSearch() =
        let broach (drone: Drone) : IAction =
            Move({ X = drone.Coordinate.X; Y = 500 }, 0)
            
        let shouldStrongLight (drone: Drone): bool =
            drone.Battery > 5
        
        let search (gameLogic: GameLogic.GameLogic) (drone: Drone) (targets: CreatureInfo list) : IAction option =
            let unknownFishes = targets |> gameLogic.UnknownFishes
            let radars = if unknownFishes.IsEmpty then [] else gameLogic.FindRadars unknownFishes
            
            if radars.IsEmpty then
                None
            else
                radars
                |> List.minBy (fun radar -> radar.CreatureId)
                |> tap (fun radar -> stderr.WriteLine $"follow: {radar.CreatureId}")
                |> fun radar -> gameLogic.FollowRadar radar drone
                |> fun coordinate -> Some (Move(coordinate, (if shouldStrongLight drone then 1 else 0)))
        
        let searchShallow (gameLogic: GameLogic.GameLogic) (drone: Drone) : IAction option =
            gameLogic.ShallowFishes
            |> search gameLogic drone
        
        let searchMiddle (gameLogic: GameLogic.GameLogic) (drone: Drone) : IAction option =
            gameLogic.MiddleFishes
            |> search gameLogic drone
        
        let searchDeep (gameLogic: GameLogic.GameLogic) (drone: Drone) : IAction option =
            gameLogic.DeepFishes
            |> search gameLogic drone

        
        interface IStrategy with
            member this.NextActions game gameData =
                let gameLogic = GameLogic.GameLogic(game, gameData)
                let sortedDrones = gameData.MyDrones |> List.sortBy (fun drone -> drone.Id)
                let drone1 = sortedDrones.Item(0)
                let drone2 = sortedDrones.Item(1)
                
                let mutable drone1Action = searchShallow gameLogic drone1
                if drone1Action.IsNone then
                    drone1Action <- searchMiddle gameLogic drone1
                if drone1Action.IsNone then
                    drone1Action <- searchDeep gameLogic drone1
                if drone1Action.IsNone then
                    drone1Action <- broach drone1 |> Some
                
                let mutable drone2Action = searchDeep gameLogic drone2
                if drone2Action.IsNone then
                    drone2Action <- searchMiddle gameLogic drone2
                if drone2Action.IsNone then
                    drone2Action <- searchShallow gameLogic drone2
                if drone2Action.IsNone then
                    drone2Action <- broach drone2 |> Some
                
                {
                    Drone1Action = drone1Action.Value
                    Drone2Action = drone2Action.Value
                }
        
module Main =
    let gamePlay() =
        let MaxTurn = 200
        let game = IOHelper.initialize
        let strategy: Strategies.IStrategy = Strategies.DepthSplitSearch()

        [1 .. MaxTurn]
        |> List.iter (fun turn -> 
            let gameData = IOHelper.readGameData MaxTurn turn
            strategy.NextActions game gameData |> IOHelper.writeAction
        )

Main.gamePlay()
