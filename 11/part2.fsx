open System
open System.IO

let loadSeatingArea inputFile =
    File.ReadAllLines inputFile
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (fun c -> c.ToString()))

let rec sumNeighbours currentRow currentColumn (currentState:string array array) =
    let minX = 0
    let maxX = (currentState.[0].Length)
    let minY = 0
    let maxY = (currentState.Length)

    let rec findSeat currentX currentY (deltaX,deltaY) =
        // let newX = if (currentX = minX) || (currentX = maxX) then currentX else currentX + deltaX
        // let newY = if (currentY = minY) || (currentY = maxY) then currentY else currentY + deltaY
        let newX = currentX + deltaX
        let newY = currentY + deltaY
        // printfn "X: %d Y: %d" newX newY
        // Console.ReadLine() |> ignore
        if (newX >= maxX) || (newX < minX) || (newY >= maxY) || (newY < minY) then 
            if (currentX = currentColumn) && (currentY = currentRow) then "" else currentState.[currentY].[currentX]
        elif currentState.[newY].[newX] = "." then 
            findSeat newX newY (deltaX,deltaY) 
        else 
            currentState.[newY].[newX]

    let findNeighoursOperation = [|(-1,-1);(0,-1);(1,-1);
                                   (-1,0);       (1,0);
                                   (-1,1);(0,1); (1,1)|]

    // let sum = findNeighoursOperation |> Array.map (findSeat currentColumn currentRow)
    //                                  |> Array.sumBy (fun place -> if place = "#" then 1 else 0)
    let sum = findNeighoursOperation |> Array.map (findSeat currentColumn currentRow)
                                     |> Array.sumBy (fun place -> if place = "#" then 1 else 0)
    sum

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
        nextState.[currentRow].[currentColumn] <- (if (sumNeighbours currentRow currentColumn currentState) >= 5 then "L" else "#")
        move
    | "L" ->
        nextState.[currentRow].[currentColumn] <- (if (sumNeighbours currentRow currentColumn currentState) <> 0 then "L" else "#")
        move

let rec playGame (currentState:string array array) =
    let newState = (Array.zeroCreate currentState.Length |> Array.map (fun _ -> Array.zeroCreate currentState.[0].Length |> Array.map (fun _ -> ".")))
    printfn "%A" currentState
    // Console.ReadLine() |> ignore
    // let gna = (Array2D.create currentState.Length currentState.[0].Length  "")
    match runRound 0 0 newState currentState with
    | state when state = currentState -> currentState
    | newState -> playGame newState

#time

loadSeatingArea "sampleInput.txt"
|> playGame
|> Array.collect(fun a -> a) |> Array.sumBy (fun s -> if s = "#" then 1 else 0)