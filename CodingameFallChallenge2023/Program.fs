open System
open System.Collections.Generic
open Microsoft.FSharp.Core

let tap f x = f x; x

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

    type Move(coordinate: GameData.Coordinate, light: int) =
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
        
        member this.VisibleMonsters: Monster list =
            gameData.VisibleCreatures
            |> List.filter (fun creature -> List.contains creature.Id (game.Monsters |> List.map (fun monster -> monster.Id)))
            |> List.map (fun creature -> { Id = creature.Id; Coordinate = creature.Coordinate; Vx = creature.Vx; Vy = creature.Vy }: Monster)
        
        member this.NearestMonster (drone: Drone): Monster option =
            this.VisibleMonsters
            |> List.sortBy (fun monster -> monster.Coordinate.DistanceTo drone.Coordinate)
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
                { X = drone.Coordinate.X - droneSpeed; Y = drone.Coordinate.Y }
            elif this.tangentLeft drone then
                { X = drone.Coordinate.X; Y = drone.Coordinate.Y - droneSpeed }
            else
                { X = drone.Coordinate.X - droneSpeed; Y = drone.Coordinate.Y - droneSpeed }
                
        member private this.goToTR (drone: Drone) =
            if this.tangentTop drone then
                { X = drone.Coordinate.X + droneSpeed; Y = drone.Coordinate.Y }
            elif this.tangentRight drone then
                { X = drone.Coordinate.X; Y = drone.Coordinate.Y - droneSpeed }
            else
                { X = drone.Coordinate.X + droneSpeed; Y = drone.Coordinate.Y - droneSpeed }
        
        member private this.goToBR (drone: Drone) =
            if this.tangentBottom drone then
                { X = drone.Coordinate.X + droneSpeed; Y = drone.Coordinate.Y }
            elif this.tangentRight drone then
                { X = drone.Coordinate.X; Y = drone.Coordinate.Y + droneSpeed }
            else
                { X = drone.Coordinate.X + droneSpeed; Y = drone.Coordinate.Y + droneSpeed }
        
        member private this.goToBL (drone: Drone) =
            if this.tangentBottom drone then
                { X = drone.Coordinate.X - droneSpeed; Y = drone.Coordinate.Y }
            elif this.tangentLeft drone then
                { X = drone.Coordinate.X; Y = drone.Coordinate.Y + droneSpeed }
            else
                { X = drone.Coordinate.X - droneSpeed; Y = drone.Coordinate.Y + droneSpeed }
        
        member this.FollowRadar (radar: RadarBlip) (drone: Drone): Coordinate =
            match radar.Radar with
            | "TL" -> this.goToTL drone
            | "TR" -> this.goToTR drone
            | "BR" -> this.goToBR drone
            | "BL" -> this.goToBL drone
            | _    -> stderr.WriteLine "レーダー情報の読み込みに失敗"; drone.Coordinate

        member this.IsMonsterNearBy (drone: Drone): bool =
            this.NearestMonster drone
            |> Option.exists (fun monster -> monster.Coordinate.DistanceTo drone.Coordinate
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

        /// モンスターの次のターンでの予測位置を計算
        member this.PredictMonsterPosition (monster: Monster): Coordinate =
            {
                X = monster.Coordinate.X + monster.Vx;
                Y = monster.Coordinate.Y + monster.Vy;
            }

        /// ドローンが安全に逃げるための座標を計算
        member this.CalculateSafeEscapeCoordinate (drone: Drone, monster: Monster): Coordinate =
            let predictedMonsterPosition = this.PredictMonsterPosition(monster)
            let deltaX = drone.Coordinate.X - predictedMonsterPosition.X
            let deltaY = drone.Coordinate.Y - predictedMonsterPosition.Y
            let escapeDistance = this.DroneSpeed
            let norm = Math.Sqrt(float(deltaX * deltaX + deltaY * deltaY))

            // 予測位置から逃避方向を計算
            let escapeX = int(float drone.Coordinate.X + float escapeDistance * (float deltaX / norm))
            let escapeY = int(float drone.Coordinate.Y + float escapeDistance * (float deltaY / norm))

            // 逃避座標をマップの範囲内に制限
            {
                X = Math.Max(0, Math.Min(this.Width, escapeX));
                Y = Math.Max(0, Math.Min(this.Height, escapeY));
            }

module Strategies =
    type IStrategy =
        abstract member NextActions: GameData.Game -> GameData.GameData -> Actions.Commands    
    type DepthSplitSearch() =
        let savingState = Dictionary<int, bool>()
        let avoidState = Dictionary<int, bool>()
        
        let setSavingState (droneId: int) (state: bool) =
            savingState.[droneId] <- state
        let getSavingState (droneId: int) =
            if savingState.ContainsKey droneId then
                savingState.[droneId]
            else
                false
        
        let setAvoidState (droneId: int) (state: bool) =
            avoidState.[droneId] <- state
        let getAvoidState (droneId: int) =
            if avoidState.ContainsKey droneId then
                avoidState.[droneId]
            else
                false
        
        let broach (drone: Drone) : IAction =
            Move({ X = drone.Coordinate.X; Y = 500 }, 0)
            
        let shouldStrongLight (drone: Drone): bool =
            drone.Battery > 5
        
        let search (gameLogic: GameLogic.GameLogic) (drone: Drone) (targets: FishInfo list) : IAction option =
            let unknownFishes = targets |> gameLogic.UnknownFishes
            let radars = if unknownFishes.IsEmpty then [] else gameLogic.FindRadars unknownFishes drone
            
            if unknownFishes.IsEmpty then
                setSavingState drone.Id true
            
            if gameLogic.HoldingScore drone > 20 then
                setSavingState drone.Id true
            
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
        
        let save (gameLogic: GameLogic.GameLogic) (drone: Drone) : IAction option =
            if getSavingState drone.Id then
                match gameLogic.hasUnSavedFish drone with
                | true -> Some (Move({ X = drone.Coordinate.X; Y = 500 }, 0))
                | false -> setSavingState drone.Id false; None
            else
                None
  
        let searchMiddle (gameLogic: GameLogic.GameLogic) (drone: Drone) : IAction option =
            gameLogic.MiddleFishes
            |> search gameLogic drone
        
        let searchDeep (gameLogic: GameLogic.GameLogic) (drone: Drone) : IAction option =
            gameLogic.DeepFishes
            |> search gameLogic drone

        let avoidMonster (gameLogic: GameLogic.GameLogic) (drone: Drone) : IAction option =
            if gameLogic.IsMonsterNearBy drone then
                setAvoidState drone.Id true
            else
                setAvoidState drone.Id false
            
            if getAvoidState drone.Id then
                let monster = gameLogic.VisibleMonsters |> List.sortBy (fun monster -> monster.Coordinate.DistanceTo drone.Coordinate) |> List.head
                let avoidCoordinate = { X = drone.Coordinate.X + monster.Vx; Y = 500 }
                Some (Move(avoidCoordinate, 0))
            else
                None
        
        interface IStrategy with
            member this.NextActions game gameData =
                let gameLogic = GameLogic.GameLogic(game, gameData)
                let sortedDrones = gameData.MyDrones |> List.sortBy (fun drone -> drone.Id)
                let drone1 = sortedDrones.Item(0)
                let drone2 = sortedDrones.Item(1)

                let mutable drone1Action = avoidMonster gameLogic drone1
                if drone1Action.IsNone then
                    drone1Action <- save gameLogic drone1
                if drone1Action.IsNone then
                    drone1Action <- searchShallow gameLogic drone1
                if drone1Action.IsNone then
                    drone1Action <- searchMiddle gameLogic drone1
                if drone1Action.IsNone then
                    drone1Action <- searchDeep gameLogic drone1
                if drone1Action.IsNone then
                    drone1Action <- broach drone1 |> Some
                
                let mutable drone2Action = avoidMonster gameLogic drone2
                if drone2Action.IsNone then
                    drone2Action <- save gameLogic drone2
                if drone2Action.IsNone then
                    drone2Action <- searchDeep gameLogic drone2
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
            
            let diveToShallow (drone: DroneBrain) : IAction * bool =
                let goal = if drone.IsLeft then { X = 2000; Y = 3000 } else { X = 8000; Y = 3000 }
                let isGoal = drone.Coordinate.DistanceTo goal < 500*500
                stderr.WriteLine $"distance: {drone.Coordinate.DistanceTo goal}"
                let light = if isGoal then 1 else 0
                Move(goal, light), isGoal
            
            let diveToMiddle (drone: DroneBrain) : IAction * bool =
                let goal = if drone.IsLeft then { X = 2000; Y = 6000 } else { X = 8000; Y = 6000 }
                let isGoal = drone.Coordinate.DistanceTo goal < 500*500
                let light = if isGoal then 1 else 0
                Move(goal, light), isGoal
            
            let diveToDeep (drone: DroneBrain) : IAction * bool =
                let goal = if drone.IsLeft then { X = 2000; Y = 7800 } else { X = 8000; Y = 7800 }
                let isGoal = drone.Coordinate.DistanceTo goal < 500*500
                let light = if isGoal then 1 else 0
                Move(goal, light), isGoal
            
            let broach (drone: DroneBrain) : IAction * bool =
                let isGoal = drone.Coordinate.Y <= 500
                Move({ X = drone.Coordinate.X; Y = 500 }, 0), isGoal
            
            let avoidMonster (gameLogic: GameLogic.GameLogic) (drone: Drone) : IAction option =
                if gameLogic.IsMonsterNearBy drone then
                    let monster = gameLogic.VisibleMonsters |> List.minBy (fun monster -> monster.Coordinate.DistanceTo(drone.Coordinate))
                    let escapeCoordinate = gameLogic.CalculateSafeEscapeCoordinate(drone, monster)
                    Some (Move(escapeCoordinate, 0))
                else
                    None
            
            member private this.SelectAction (droneBrain: DroneBrain) (gameLogic: GameLogic.GameLogic): IAction =
                avoidMonster gameLogic droneBrain.Drone
                |> (fun action -> match action with
                                  | None -> match droneBrain.State with
                                            | DiveToShallow -> diveToShallow droneBrain |> fun (action, isGoal) -> if isGoal then droneBrain.SetState DiveToMiddle; action else action
                                            | DiveToMiddle -> diveToMiddle droneBrain |> fun (action, isGoal) -> if isGoal then droneBrain.SetState DiveToDeep; action else action
                                            | DiveToDeep -> diveToDeep droneBrain |> fun (action, isGoal) -> if isGoal then droneBrain.SetState Broach; action else action
                                            | Broach -> broach droneBrain |> fun (action, isGoal) -> if isGoal then droneBrain.SetState DiveToShallow; action else action
                                            |> tap (fun _ -> stderr.WriteLine $"state: {droneBrain.State}")
                                  | Some value -> value)
            
            interface IStrategy with
                member this.NextActions game gameData =
                    let gameLogic = GameLogic.GameLogic(game, gameData)
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
