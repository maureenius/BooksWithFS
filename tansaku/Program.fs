open System
open tansaku
open tansaku.BoardGame.Scores
open tansaku.Strategy

let playGame (strategy: IStrategy) =
    Game(strategy).Play()

let playMultipleGames (strategy: IStrategy) (n: int) =
    let mutable totalScore: Score = { Value = 0 }
    for i in 1..n do
        let game = Game(strategy)
        game.Play()
        totalScore <- totalScore + game.Score
    
    let average = float totalScore.Value / float n
    printfn $"Average score: %f{average}"

[<EntryPoint>]
let main args =
    let strategy = Random()
    
    match args with
    | [| "-single" |] -> 
        playGame strategy
        0  // 正常終了
    | [| "-average"; times |] ->
        let mutable timesInt = 0
        if Int32.TryParse(times, &timesInt) then
            playMultipleGames strategy timesInt
            0  // 正常終了
        else
            printfn "Invalid number of times for average mode."
            1  // エラー終了
    | _ ->
        printfn "Usage: \n  -single : Play single game\n  -average [times] : Play multiple games and show average score"
        1  // エラー終了
