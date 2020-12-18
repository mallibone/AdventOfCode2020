#time
open System
open System.IO

let parseBusLines (rawString:string) =
    rawString.Replace("x,", "").Split(",") |> Array.map Convert.ToInt32

let parseInput inputFile =
    let lines = File.ReadAllLines(inputFile)

    let earliestTime = Convert.ToInt32(lines.[0])
    let busLines = parseBusLines lines.[1]

    (earliestTime, busLines)

let findEarliestDeparture (earliestTime, busLines) =
    let rec findDeparture departureTime =
        let bus = busLines |> Array.tryFind (fun busLine -> (departureTime % busLine) = 0)
        match bus with
        | Some line -> (departureTime - earliestTime) * line
        | None -> findDeparture (departureTime+1)

    findDeparture earliestTime

// parseInput "sampleInput.txt"
parseInput "input.txt"
|> findEarliestDeparture