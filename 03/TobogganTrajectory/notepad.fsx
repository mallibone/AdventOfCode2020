open System
open System.IO

#time
let readInput filename =
    File.ReadAllLines(filename)

let createMap (right, down) (originalMap:string []) =
    let width = (originalMap.[0]).Length
    let height = originalMap |> Array.length
    let steps = height/down
    let mapParts = (right*steps/width)+1
    originalMap |> Array.map (fun line -> String.Concat([| for _ in 1 .. mapParts -> line |]))

let traverseMap (right, down) (map:string []) =
    let rec traverse (x, y) positions =
        let position = map.[y].[x]
        let mapEnd = (map |> Array.length)-1
        // printfn "%c found at %d/%d" position x y
        match y with
        | y when y = mapEnd -> (position::positions)
        | _ -> traverse (x+right, y+down) (position::positions)
    traverse (0,0) []


// let steps = (3,1)
// let paths = [(3,1)] // part 1jjjjjgg
let paths = [(1,1);(3,1);(5,1);(7,1);(1,2)]

let findTreas input steps =
    input |> createMap steps |> traverseMap steps |> List.filter (fun c -> c='#') |> List.length

paths |> List.map (findTreas (readInput "testInput.txt")) |> List.fold (*) 1
paths |> List.map (findTreas (readInput "input.txt") >> Convert.ToUInt32) |> List.fold (*) 1u