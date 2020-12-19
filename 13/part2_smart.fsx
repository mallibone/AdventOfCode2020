#time
open System
open System.IO

let parseBusLines (rawString:string) =
    rawString.Replace("x", "1").Split(",")
    |> Array.mapi (fun i v -> int64 i, Convert.ToInt64(v))
    |> Array.filter (snd >> ((<) 1L))
    |> Array.toList

let parseInput inputFile =
    let lines = File.ReadAllLines(inputFile)
    parseBusLines lines.[1]

let rec findEarliestDeparture (departureTime:int64) (stepSize) (busLines:(int64 * int64) List) =
     let rec findTime (time:int64) stepSize (offset, busLine) =
         if (time + offset) % busLine = 0L then
            time
         else
             findTime (time+stepSize) stepSize (offset, busLine)

     match busLines with
     | currentLine::lines ->
         let newDepartureTime = findTime departureTime stepSize currentLine
         findEarliestDeparture newDepartureTime (stepSize*(snd currentLine)) lines
    //  | [lastLine] ->
    //      findTime departureTime stepSize lastLine
     | [] -> departureTime

parseInput "Input.txt"
// parseInput "sampleInput.txt"
|> findEarliestDeparture 0L 1L
