open System
open tansaku
open tansaku.BoardGame.Scores
open tansaku.Strategy

let playGame (strategy: IStrategy) =
    Game(strategy).Play()

let playMultipleGames (strategy: IStrategy) (n: int): float =
    let mutable totalScore: Score = { Value = 0 }
    for i in 1..n do
        let game = Game(strategy)
        game.Play()
        totalScore <- totalScore + game.Score
    
    let average = float totalScore.Value / float n
    printfn $"Average score: %f{average}"
    
    average

let compare (strategies: IStrategy list) (times: int) =
    List.map (fun strategy -> (strategy, playMultipleGames strategy times)) strategies
    |> List.sortByDescending snd
    |> List.iter (fun (strategy, score) -> printfn $"%s{strategy.Name}: %f{score}")

[<EntryPoint>]
let main args =
    let strategy = Greedy()
    
    match args with
    | [| "-single" |] -> 
        playGame strategy
        0  // 正常終了
    | [| "-average"; times |] ->
        let mutable timesInt = 0
        if Int32.TryParse(times, &timesInt) then
            playMultipleGames strategy timesInt |> ignore
            0  // 正常終了
        else
            printfn "Invalid number of times for average mode."
            1  // エラー終了
    | [| "-compare"; times |] ->
        let mutable timesInt = 0
        if Int32.TryParse(times, &timesInt) then
            compare [Random(); Greedy()] timesInt
            0  // 正常終了
        else
            printfn "Invalid number of times for average mode."
            1  // エラー終了
    | _ ->
        printfn "Usage: \n  -single : Play single game\n  -average [times] : Play multiple games and show average score"
        1  // エラー終了
