open System
open System.Collections.Generic
open Microsoft.FSharp.Core

let tap f x = f x; x

module CoordinateSystem =
    type Coordinate =
        {
            X: int
            Y: int
        }
    
    let SquaredDistance (coordinate1: Coordinate) (coordinate2: Coordinate): int =
        let dx = coordinate1.X - coordinate2.X
        let dy = coordinate1.Y - coordinate2.Y
        dx*dx + dy*dy
    
    let Distance (coordinate1: Coordinate) (coordinate2: Coordinate): float =
        SquaredDistance coordinate1 coordinate2 |> float |> sqrt

open CoordinateSystem

module VectorOperation =
    type Vector =
        { X: float; Y: float }
        member this.ToCoordinate: Coordinate =
            { X = int(this.X); Y = int(this.Y) }
        member this.Norm: float =
            sqrt(this.X*this.X + this.Y*this.Y)
        
    let add (vector1: Vector) (vector2: Vector): Vector =
        { X = vector1.X + vector2.X; Y = vector1.Y + vector2.Y }
    let subtract (vector1: Vector) (vector2: Vector): Vector =
        { X = vector1.X - vector2.X; Y = vector1.Y - vector2.Y }
    let multiply (vector: Vector) (scalar: float): Vector =
        { X = vector.X * scalar; Y = vector.Y * scalar }
    let dot (vector1: Vector) (vector2: Vector): float =
        vector1.X*vector2.X + vector1.Y*vector2.Y
    let norm (vector: Vector): float =
        sqrt(vector.X*vector.X + vector.Y*vector.Y)
    
    let merge (vectors: Vector list): Vector =
        vectors |> List.fold add { X = 0.0; Y = 0.0 }
    let normalize (vector: Vector): Vector =
        let norm = sqrt(vector.X*vector.X + vector.Y*vector.Y)
        { X = vector.X/norm; Y = vector.Y/norm }

open VectorOperation

module CollisionDetection =
    let droneRadius = 200.0 // 仮定するドローンの半径
    let monsterRadius = 300.0 // 仮定するモンスターの半径
    let collisionThreshold = droneRadius + monsterRadius

    // 二つの線分間の最短距離を計算
    let private lineSegmentDistance (p1: Vector) (p2: Vector) (q1: Vector) (q2: Vector): float =
        // ヘルパー関数: 点と線分間の最短距離
        let pointToLineSegmentDistance p q1 q2 =
            let qp = VectorOperation.subtract p q1
            let qq = VectorOperation.subtract q2 q1
            let t = max 0.0 (min 1.0 (VectorOperation.dot qp qq / VectorOperation.dot qq qq))
            VectorOperation.subtract qp (VectorOperation.multiply qq t) |> VectorOperation.norm

        min (pointToLineSegmentDistance p1 q1 q2)
            (min (pointToLineSegmentDistance p2 q1 q2)
                (min (pointToLineSegmentDistance q1 p1 p2)
                    (pointToLineSegmentDistance q2 p1 p2)))

    let willCollide (dronePosition: Vector) (monsterPosition: Vector) (droneMoveVector: Vector) (monsterMoveVector: Vector): bool =
        // 次のターンでの予想位置
        let droneNextPos = VectorOperation.add dronePosition droneMoveVector
        let monsterNextPos = VectorOperation.add monsterPosition monsterMoveVector

        // 軌跡間の最短距離
        let distance = lineSegmentDistance dronePosition droneNextPos monsterPosition monsterNextPos

        // 衝突判定
        distance <= collisionThreshold

module GameData =
    type FishInfo =
        {
            Id : int
            Color : int
            Type : int
        }
        member this.BaseScore =
            match this.Type with
            | 0 -> 1
            | 1 -> 2
            | 2 -> 3
            | _ -> 0
    
    type MonsterInfo = {
        Id: int
    }

    type Game = {
        Fishes: FishInfo list
        Monsters: MonsterInfo list
    }

    type Creature = {
        Id: int
        Coordinate: Coordinate
        Vx: int
        Vy: int
    }
    
    type Fish = Creature
    type Monster = Creature

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
        VisibleCreatures: Fish list
        RadarBlips: RadarBlip list
        CurrentTurn: int
        MaxTurn: int
    }

open GameData

module Actions =
    type IAction =
        abstract member Output: unit -> string

    type Commands = {
        Drone1Action: IAction
        Drone2Action: IAction
    }
    
    type Wait(light: int) =
        interface IAction with
            member this.Output() = $"WAIT {light} Drifting..."

    type Move(coordinate: Coordinate, light: int) =
        interface IAction with
            member this.Output() = $"MOVE {coordinate.X} {coordinate.Y} {light} MOVING!"

open Actions

module IOHelper =
    let readInt() = Console.In.ReadLine() |> int
    
    let initialize: GameData.Game = 
        let creatureCount = readInt()
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
            Fishes = creatures |> List.filter (fun creature -> creature.Type <> -1)
            Monsters = creatures |> List.filter (fun creature -> creature.Type = -1) |> List.map (fun creature -> { Id = creature.Id })
        }

    let readDroneScans (count: int): GameData.DroneScan list =
        [0 .. count - 1]
        |> List.map (fun _ -> Console.In.ReadLine())
        |> List.map (fun line -> line.Split [|' '|])
        |> List.map (fun token -> { DroneId = int(token[0]); CreatureId = int(token[1]) })
        |> tap (fun scans -> stderr.WriteLine $"scans: {scans}")

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
        |> tap (fun creatures -> stderr.WriteLine $"creatures: {creatures}")

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
        |> tap (fun drones -> stderr.WriteLine $"drones: {drones}")

    let readRadars (count: int): GameData.RadarBlip list = 
        [0 .. count - 1]
        |> List.map (fun _ -> Console.In.ReadLine())
        |> List.map (fun line -> line.Split [|' '|])
        |> List.map (fun token -> {
            DroneId = int(token[0])
            CreatureId = int(token[1])
            Radar = token[2]
        })
        |> tap (fun radars -> stderr.WriteLine $"radars: {radars}")

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
        Console.Out.WriteLine(commands.Drone1Action.Output()) |> tap (fun _ -> stderr.WriteLine (commands.Drone1Action.Output()))
        Console.Out.WriteLine(commands.Drone2Action.Output()) |> tap (fun _ -> stderr.WriteLine (commands.Drone2Action.Output()))

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
       
        member this.CurrentTurn = gameData.CurrentTurn
        
        override this.ToString() =
            $"{game}\n{gameData}"
        
        member this.ShallowFishes =
            game.Fishes |> List.filter (fun creature -> creature.Type = 0)
        
        member this.IsShallowClear = this.ShallowFishes |> List.isEmpty
        
        member this.MiddleFishes =
            game.Fishes |> List.filter (fun creature -> creature.Type = 1)
            
        member this.IsMiddleClear = this.MiddleFishes |> List.isEmpty
            
        member this.DeepFishes =
            game.Fishes |> List.filter (fun creature -> creature.Type = 2)
        
        member this.IsDeepClear = this.DeepFishes |> List.isEmpty
            
        member this.FindRadars (targets: FishInfo list) (drone: Drone): RadarBlip list =
            gameData.RadarBlips
            |> List.filter (fun blip -> blip.DroneId = drone.Id)
            |> List.filter (fun blip -> List.contains blip.CreatureId (targets |> List.map (fun fish -> fish.Id)))

        member this.FindMonster (creatureId: int): Monster option =
            gameData.VisibleCreatures |> List.tryFind (fun creature -> creature.Id = creatureId)
        
        member this.IsSavedByMe (creature: FishInfo) =
            List.contains creature.Id gameData.MyScanCount
        
        member this.IsSavedByFoe (creature: FishInfo) =
            List.contains creature.Id gameData.FoeScanCount
        
        member this.IsScannedAndUnsavedByMe (creatureId: int) =
            gameData.DroneScans
            |> List.filter (fun scan -> List.contains scan.DroneId (gameData.MyDrones |> List.map (fun drone -> drone.Id)))
            |> List.exists (fun scan -> scan.CreatureId = creatureId)
        
        member this.IsScannedByMe (creature: FishInfo) =
            this.IsSavedByMe creature || this.IsScannedAndUnsavedByMe creature.Id
        
        member this.UnknownFishes (targets: FishInfo list): FishInfo list =
            targets
            |> List.filter (fun creature -> this.IsScannedByMe creature |> not) 

        member this.hasUnSavedFish (drone: Drone): bool =
            gameData.DroneScans
            |> List.filter (fun scan -> scan.DroneId = drone.Id)
            |> List.exists (fun scan -> this.IsScannedAndUnsavedByMe scan.CreatureId)
        
        member this.AllMonsterIds: int list =
            game.Monsters |> List.map (fun monster -> monster.Id)
        
        member this.VisibleMonsters: Monster list =
            gameData.VisibleCreatures
            |> List.filter (fun creature -> List.contains creature.Id (game.Monsters |> List.map (fun monster -> monster.Id)))
            |> List.map (fun creature -> { Id = creature.Id; Coordinate = creature.Coordinate; Vx = creature.Vx; Vy = creature.Vy }: Monster)
        
        member this.NearestMonster (drone: Drone): Monster option =
            this.VisibleMonsters
            |> List.sortBy (fun monster -> SquaredDistance monster.Coordinate drone.Coordinate)
            |> List.tryHead
        
        member private this.tangentBottom (drone: Drone): bool =
            drone.Coordinate.Y >= height - droneSpeed
        
        member private this.tangentTop (drone: Drone): bool =
            drone.Coordinate.Y <= droneSpeed
        
        member private this.tangentLeft (drone: Drone): bool =
            drone.Coordinate.X <= droneSpeed
        
        member private this.tangentRight (drone: Drone): bool =
            drone.Coordinate.X >= width - droneSpeed
        
        member private this.goToTL (drone: Drone) =
            if this.tangentTop drone then
                { X = -1.0; Y = 0 }
            elif this.tangentLeft drone then
                { X = 0; Y = -1.0 }
            else
                { X = -0.5; Y = -0.5 }
                
        member private this.goToTR (drone: Drone) =
            if this.tangentTop drone then
                { X = 1.0; Y = 0 }
            elif this.tangentRight drone then
                { X = 0; Y = -1.0 }
            else
                { X = 0.5; Y = -0.5 }
                
        member private this.goToBR (drone: Drone) =
            if this.tangentBottom drone then
                { X = 1.0; Y = 0 }
            elif this.tangentRight drone then
                { X = 0; Y = 1.0 }
            else
                { X = 0.5; Y = 0.5 }
                
        member private this.goToBL (drone: Drone) =
            if this.tangentBottom drone then
                { X = -1.0; Y = 0 }
            elif this.tangentLeft drone then
                { X = 0; Y = 1.0 }
            else
                { X = -0.5; Y = 0.5 }
        
        member this.FollowRadar (radar: RadarBlip) (drone: Drone): Vector =
            match radar.Radar with
            | "TL" -> this.goToTL drone
            | "TR" -> this.goToTR drone
            | "BR" -> this.goToBR drone
            | "BL" -> this.goToBL drone
            | _    -> stderr.WriteLine "レーダー情報の読み込みに失敗"; { X = float drone.Coordinate.X; Y = float drone.Coordinate.Y }

        member this.IsMonsterNearBy (drone: Drone): bool =
            this.NearestMonster drone
            |> Option.exists (fun monster -> SquaredDistance monster.Coordinate drone.Coordinate
                                              |> tap (fun distance -> stderr.WriteLine $"monster distance: {distance}")
                                              |> fun distance -> distance <= 1200*1200)

        member private this.WillBingoHorizontal (fishes: FishInfo list) (color: int): bool =
            let targetColorFishes = fishes |> List.filter (fun fish -> fish.Color = color)
            let savedColorFishes = targetColorFishes |> List.filter this.IsSavedByMe
            let isAlreadyBingo = savedColorFishes.Length = 3
            let willBingo = not isAlreadyBingo && savedColorFishes.Length + fishes.Length = 3
            
            willBingo
        
        member private this.IsBingoFoeHorizontal (color: int): bool =
            gameData.FoeScanCount
             |> List.map (fun creatureId -> game.Fishes |> List.find (fun fish -> fish.Id = creatureId))
             |> List.filter (fun fish -> fish.Color = color)
             |> List.length = 3
        
        member private this.WillBingoVertical (fishes: FishInfo list) (fishType: int): bool =
            let targetTypeFishes = fishes |> List.filter (fun fish -> fish.Type = fishType)
            let savedTypeFishes = targetTypeFishes |> List.filter this.IsSavedByMe
            let isAlreadyBingo = savedTypeFishes.Length = 4
            let willBingo = not isAlreadyBingo && savedTypeFishes.Length + fishes.Length = 4
            
            willBingo
        
        member this.IsBingoFoeVertical (fishType: int): bool =
            gameData.FoeScanCount
             |> List.map (fun creatureId -> game.Fishes |> List.find (fun fish -> fish.Id = creatureId))
             |> List.filter (fun fish -> fish.Type = fishType)
             |> List.length = 4
        
        member this.HoldingScore (drone: Drone): int =
            let fishes = gameData.DroneScans
                         |> List.filter (fun scan -> scan.DroneId = drone.Id)
                         |> List.filter (fun scan -> this.IsScannedAndUnsavedByMe scan.CreatureId)
                         |> List.map (fun scan -> game.Fishes |> List.find (fun fish -> fish.Id = scan.CreatureId))
            let mutable score = fishes |> List.sumBy (fun fish -> if this.IsSavedByMe fish || this.IsSavedByFoe fish then fish.BaseScore else fish.BaseScore*2)
            
            if this.WillBingoHorizontal fishes 0 then
                if this.IsBingoFoeHorizontal 0 then
                    score <- score + 3
                else
                    score <- score + 6
            if this.WillBingoHorizontal fishes 1 then
                if this.IsBingoFoeHorizontal 1 then
                    score <- score + 3
                else
                    score <- score + 6
            if this.WillBingoHorizontal fishes 2 then
                if this.IsBingoFoeHorizontal 2 then
                    score <- score + 3
                else
                    score <- score + 6
            if this.WillBingoVertical fishes 0 then
                if this.IsBingoFoeVertical 0 then
                    score <- score + 4
                else
                    score <- score + 8
            if this.WillBingoVertical fishes 1 then
                if this.IsBingoFoeVertical 1 then
                    score <- score + 4
                else
                    score <- score + 8
            if this.WillBingoVertical fishes 2 then
                if this.IsBingoFoeVertical 2 then
                    score <- score + 4
                else
                    score <- score + 8
            if this.WillBingoVertical fishes 3 then
                if this.IsBingoFoeVertical 3 then
                    score <- score + 4
                else
                    score <- score + 8
            if this.WillBingoVertical fishes 4 then
                if this.IsBingoFoeVertical 4 then
                    score <- score + 4
                else
                    score <- score + 8
            
            score

module DroneLogic =
    let speed = 600
    let destinationVector (drone: Drone) (destination: Coordinate): Vector =
        let dx = float(destination.X - drone.Coordinate.X)
        let dy = float(destination.Y - drone.Coordinate.Y)
        { X = dx; Y = dy } |> normalize

    let moveCoordinate (drone: Drone) (vector: Vector): Coordinate =
        { X = int(float drone.Coordinate.X + vector.X); Y = int(float drone.Coordinate.Y + vector.Y) }

module MonsterLogic =
    let monsterYLimit = 2500
    
    let predictNextPosition (monster: Monster): Coordinate =
        {
            X = max 0 (min 10000 (monster.Coordinate.X + monster.Vx));
            Y = max monsterYLimit monster.Coordinate.Y + monster.Vy
        }
    
    let monsterAvoidanceVector (drone: Drone) (monster: Monster): Vector =
        let dx = float(drone.Coordinate.X - monster.Coordinate.X)
        let dy = float(drone.Coordinate.Y - monster.Coordinate.Y)
        { X = dx; Y = dy } |> normalize

module Strategies =
    type IStrategy =
        abstract member NextActions: GameData.Game -> GameData.GameData -> Actions.Commands

    module Peake =
        type DroneState =
            | DiveToShallow
            | DiveToMiddle
            | DiveToDeep
            | Broach
        
        type DroneBrain(_drone: Drone) =
            let mutable drone = _drone
            let mutable state = DiveToShallow
            let isLeft = drone.Coordinate.X < 5000

            member this.State = state
            member this.SetState (newState: DroneState) = state <- newState
            member this.Coordinate = drone.Coordinate
            member this.IsLeft = isLeft
            member this.Drone = drone
            member this.Update (newDrone: Drone) = drone <- newDrone
        
        type PeakeStrategy() =
            let mutable isInitialized = false
            let mutable droneBrain1: DroneBrain option = None
            let mutable droneBrain2: DroneBrain option = None
            let mutable monsterMemories: (Monster * int) list = []
            
            let moveVector (drone: Drone) (destinationVector: Vector) (monsterVectors: Vector list): Vector =
                destinationVector :: monsterVectors
                |> VectorOperation.merge
                |> VectorOperation.normalize
                |> fun vector -> VectorOperation.multiply vector (float DroneLogic.speed)
            
            let monsterVectors (drone: Drone) (monsters: Monster list): Vector list =
                monsters
                |> List.filter (fun monster -> Distance drone.Coordinate monster.Coordinate < 1000)
                |> List.map (MonsterLogic.monsterAvoidanceVector drone)
            
            let isReachDestination (drone: Drone) (destination: Coordinate): bool =
                Distance drone.Coordinate destination < 500
            
            let shallowDestination (drone: DroneBrain): Coordinate =
                if drone.IsLeft then { X = 2000; Y = 3000 } else { X = 8000; Y = 3000 }
            
            let MiddleDestination (drone: DroneBrain): Coordinate =
                if drone.IsLeft then { X = 2000; Y = 6000 } else { X = 8000; Y = 6000 }
            
            let DeepDestination (drone: DroneBrain): Coordinate =
                if drone.IsLeft then { X = 2000; Y = 7800 } else { X = 8000; Y = 7800 }
            
            let BroachDestination (drone: DroneBrain): Coordinate =
                { X = drone.Coordinate.X; Y = 500 }
            
            let threatMonsters (drone: DroneBrain): Monster list =
                monsterMemories
                |> List.filter (fun (monster, _) -> Distance drone.Coordinate monster.Coordinate < 2000)
                |> List.map fst
            
            let move (drone: DroneBrain) (destination: Coordinate) (gameLogic: GameLogic.GameLogic): IAction * bool =
                stderr.WriteLine $"droneId: {drone.Drone.Id}"
                let destinationVector = DroneLogic.destinationVector drone.Drone destination |> tap (fun vector -> stderr.WriteLine $"destinationVector: {vector}")
                let monsterVectors = threatMonsters drone |> monsterVectors drone.Drone |> tap (fun vectors -> stderr.WriteLine $"monsterVectors: {vectors}")
                let resultVector = moveVector drone.Drone destinationVector monsterVectors |> tap (fun vector -> stderr.WriteLine $"resultVector: {vector}")
                let reach = isReachDestination drone.Drone destination |> tap (fun isReach -> stderr.WriteLine $"isReach: {isReach}")

                Move(DroneLogic.moveCoordinate drone.Drone resultVector, if reach then 1 else 0), reach
            
            let diveToShallow (drone: DroneBrain) (gameLogic: GameLogic.GameLogic): IAction * bool =
                move drone (shallowDestination drone) gameLogic
            
            let diveToMiddle (drone: DroneBrain) (gameLogic: GameLogic.GameLogic): IAction * bool =
                move drone (MiddleDestination drone) gameLogic
            
            let diveToDeep (drone: DroneBrain) (gameLogic: GameLogic.GameLogic): IAction * bool =
                move drone (DeepDestination drone) gameLogic
            
            let broach (drone: DroneBrain) (gameLogic: GameLogic.GameLogic): IAction * bool =
                move drone (BroachDestination drone) gameLogic
            
            let update (gameLogic: GameLogic.GameLogic): unit =
                // 現在のターンで観測されたモンスターの記録を更新
                gameLogic.VisibleMonsters
                |> List.iter (fun monster ->
                    // 同じ ID の古い記録を削除
                    monsterMemories <- monsterMemories |> List.filter (fun (m, _) -> m.Id <> monster.Id)
                    // 新しい記録を追加
                    monsterMemories <- (monster, gameLogic.CurrentTurn) :: monsterMemories
                )

                // 古い記録を削除
                let thresholdTurn = gameLogic.CurrentTurn - 10
                monsterMemories <- monsterMemories |> List.filter (fun (_, turn) -> turn > thresholdTurn)
                
                stderr.WriteLine $"monsterMemories: {monsterMemories}"
            
            member private this.SelectAction (droneBrain: DroneBrain) (gameLogic: GameLogic.GameLogic): IAction =
                match droneBrain.State with
                | DiveToShallow -> diveToShallow droneBrain gameLogic |> fun (action, isGoal) -> if isGoal then droneBrain.SetState DiveToMiddle; action else action
                | DiveToMiddle -> diveToMiddle droneBrain gameLogic |> fun (action, isGoal) -> if isGoal then droneBrain.SetState DiveToDeep; action else action
                | DiveToDeep -> diveToDeep droneBrain gameLogic |> fun (action, isGoal) -> if isGoal then droneBrain.SetState Broach; action else action
                | Broach -> broach droneBrain gameLogic |> fun (action, isGoal) -> if isGoal then droneBrain.SetState DiveToShallow; action else action
                |> tap (fun _ -> stderr.WriteLine $"state: {droneBrain.State}")
            
            interface IStrategy with
                member this.NextActions game gameData =
                    let gameLogic = GameLogic.GameLogic(game, gameData) |> tap update

                    let drones = gameData.MyDrones |> List.sortBy (fun drone -> drone.Id)
                    if not isInitialized then
                        droneBrain1 <- drones.Item(0) |> DroneBrain |> Some
                        droneBrain2 <- drones.Item(1) |> DroneBrain |> Some
                        isInitialized <- true
                    else
                        drones.Item(0) |> droneBrain1.Value.Update
                        drones.Item(1) |> droneBrain2.Value.Update
                    
                    {
                        Drone1Action = this.SelectAction droneBrain1.Value gameLogic
                        Drone2Action = this.SelectAction droneBrain2.Value gameLogic
                    }
                          
module Main =
    let gamePlay() =
        let MaxTurn = 200
        let game = IOHelper.initialize
        let strategy: Strategies.IStrategy = Strategies.Peake.PeakeStrategy()

        [1 .. MaxTurn]
        |> List.iter (fun turn -> 
            let gameData = IOHelper.readGameData MaxTurn turn
            strategy.NextActions game gameData |> IOHelper.writeAction
        )

Main.gamePlay()
