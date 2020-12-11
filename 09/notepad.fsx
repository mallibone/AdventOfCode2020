#time
open System
open System.IO

let pairs (collection:int64 list) =
    let rec pairMaker collection pairs =
        match collection with
        | number::tail ->
            let newPairs = tail |> List.map (fun n -> (number, n))
            pairMaker tail (pairs |> List.append newPairs)
        | _ -> pairs
    
    pairMaker collection []

let findAnomaly windowSize numbers =
    let anomaly window =
        let number = Seq.last window
        let numbers = window |> Seq.rev |> Seq.skip 1 |> Seq.toList
        numbers |> pairs |> Seq.map(fun (a, b) -> a+b) 
        |> Seq.contains number |> not

    numbers 
    |> Seq.windowed (windowSize + 1)
    |> Seq.find anomaly
    |> Seq.last
    
/// Part 1
// test
File.ReadAllLines "testInput.txt"
|> Array.map Convert.ToInt64
|> findAnomaly 5

File.ReadAllLines "input.txt"
|> Array.map Convert.ToInt64
|> findAnomaly 25

let findContiguousSet numbers anomalyNumber =
    let matchingSum (window:int64 array) =
        ((window |> Array.sum) = anomalyNumber)

    let rec finder (windowSize:int) =
        let hasFoundWindow = numbers |> Seq.windowed windowSize
                                     |> Seq.tryFind matchingSum
        match hasFoundWindow with
        | Some window -> window
        | None ->
            let newWindowSize = windowSize + 1
            finder newWindowSize
    finder 2
    
let sumMinMax (numbers:int64 seq) =
    ((numbers |> Seq.min) + (numbers |> Seq.max))

let numbers = File.ReadAllLines "input.txt"
//let numbers = File.ReadAllLines "input.txt"
              |> Array.map Convert.ToInt64

numbers
|> findAnomaly 25
|> findContiguousSet numbers
|> sumMinMax