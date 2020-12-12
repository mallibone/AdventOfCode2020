open System
open System.IO
open System.Collections.Generic

let parseInput inputFile = 
    let fileInput = File.ReadAllLines(inputFile) |> Array.map Convert.ToInt64 

    fileInput 
    |> Array.append [|0L; (fileInput |> Array.max) + 3L|]
    |> Array.sort

let possibleCombinations adapterChain =
    adapterChain
    |> Array.map(fun adapter -> 
        let possibleNextAdapters = adapterChain |> Array.filter(fun na -> na > adapter && na <= adapter + 3L)
        adapter, possibleNextAdapters)
    |> Map

let rec countPossibleCombinations (possibleCombinations:Map<_,_>) (cache:IDictionary<_,_>) (currentAdapter:Int64) =
    match possibleCombinations.[currentAdapter] with
    | [||] -> 1L
    | combinations ->
        combinations
        |> Array.sumBy(fun combination ->
                            if not (cache.ContainsKey combination) then
                                cache.Add (combination, countPossibleCombinations possibleCombinations cache combination)
                            cache.[combination]
        )

#time
// let adapters = parseInput "testInput.txt"
let adapters = parseInput "input.txt"
let combinations = adapters |> possibleCombinations
countPossibleCombinations combinations  (Dictionary()) adapters.[0]
