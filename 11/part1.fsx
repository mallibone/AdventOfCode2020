open System
open System.IO

let loadSeatingArea inputFile =
    File.ReadAllLines inputFile
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (fun c -> c.ToString()))

let sumNeighbours currentRow currentColumn (currentState:string array array) =
    let startColumn = if currentColumn = 0 then currentColumn else currentColumn - 1
    let endColumn = if currentColumn = (currentState.[0].Length-1) then currentColumn else currentColumn + 1
    let startRow = if currentRow = 0 then currentRow else currentRow - 1
    let endRow = if currentRow = (currentState.Length-1) then currentRow else currentRow + 1

    let sum = currentState.[startRow .. endRow] 
                            |> Array.collect (fun line -> line.[startColumn .. endColumn])
                            |> Array.sumBy (fun place -> if place = "#" then 1 else 0)
    if currentState.[currentRow].[currentColumn] = "#" then (sum - 1) else sum

let rec runRound currentRow currentColumn (nextState:string array array) (currentState:string array array) =

    let move = 
        let maxRow = (currentState.Length-1)
        let maxColumn = (currentState.[0].Length-1)
        match (currentRow, currentColumn) with
        | row, column when row = maxRow && column = maxColumn -> nextState
        | _, column when column = maxColumn -> 
            runRound (currentRow+1) 0 nextState currentState
        | _, _ ->
            runRound currentRow (currentColumn+1) nextState currentState

    match currentState.[currentRow].[currentColumn] with
    | "." -> 
        move
    | "#" ->
        nextState.[currentRow].[currentColumn] <- (if (sumNeighbours currentRow currentColumn currentState) >= 4 then "L" else "#")
        move
    | "L" ->
        nextState.[currentRow].[currentColumn] <- (if (sumNeighbours currentRow currentColumn currentState) <> 0 then "L" else "#")
        move

let rec playGame (currentState:string array array) =
    // printfn "%A" currentState
    // System.Console.ReadLine() |> ignore
    let newState = (Array.zeroCreate currentState.Length |> Array.map (fun _ -> Array.zeroCreate currentState.[0].Length |> Array.map (fun _ -> ".")))
    // let gna = (Array2D.create currentState.Length currentState.[0].Length  "")
    match runRound 0 0 newState currentState with
    | state when state = currentState -> currentState
    | newState  -> playGame newState

#time

loadSeatingArea "input.txt"
|> playGame
|> Array.collect(fun a -> a) |> Array.sumBy (fun s -> if s = "#" then 1 else 0)