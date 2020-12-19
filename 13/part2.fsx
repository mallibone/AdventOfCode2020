#time
open System
open System.IO

let parseBusLines (rawString:string) =
    rawString.Replace("x", "1").Split(",") |> Array.map Convert.ToInt64

let parseInput inputFile =
    let lines = File.ReadAllLines(inputFile)
    parseBusLines lines.[1]

let findEarliestDeparture (departureTime:int64) busLines =
    let rec findDeparture (time:int64) =
        let depatureTimes = [| time .. (time+(int64 (busLines |> Seq.length))-1L) |]
        let gna = depatureTimes |> Array.zip busLines
        if (gna |> Array.forall (fun (b,t) -> (t % b) = 0L)) then
            time
        else
            findDeparture (time+1L)
    findDeparture departureTime

// parseInput "sampleInput.txt"
parseInput "input.txt"
// |> findEarliestDeparture 1068773
|> findEarliestDeparture 100000000000000L