#time
open System.IO

type CubeState = Active | Inactive
type CubeMap = Map<int*int*int*int,CubeState>

let input = File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input.txt"
let sampleInput = File.ReadAllLines $"{__SOURCE_DIRECTORY__}/sampleInput.txt"

let parseInput (input:string array) : CubeMap =
    input
    |> Seq.mapi (fun y line -> 
                    line.ToCharArray() 
                    |> Seq.mapi(fun x c -> (0,0,y,x), (if c = '#' then Active else Inactive)))
    |> Seq.collect (id)
    |> Map.ofSeq

let neighbours0 =
    [
        for w in -1 .. 1 do
        for z in -1 .. 1 do
        for y in -1 .. 1 do
        for x in -1 .. 1 do
        (w,z,y,x)
    ]
    |> Seq.except [(0,0,0,0)]
    |> Seq.toList

let neighbours (w,z,y,x) =
    neighbours0
    |> Seq.map (fun (w0,z0,y0,x0) -> (w0+w,z0+z,y0+y,x0+x))

let possibleDimensions (currentDimensions:CubeMap) =
    let coords = currentDimensions |> Map.toSeq |> Seq.map fst

    coords
    |> Seq.collect neighbours
    |> Seq.append coords
    |> Seq.distinct

let getCubeState (pocketState:CubeMap) (coords:int*int*int*int) =
    pocketState 
    |> Map.tryFind coords
    |> Option.defaultValue Inactive

let applyRules (pocketState:CubeMap) coords =
    let activeNeighbours = 
        neighbours coords
        |> Seq.map (getCubeState pocketState)
        |> Seq.filter ((=) Active)
        |> Seq.length
    
    let currentState =
        getCubeState pocketState coords
    let nextState =
        match activeNeighbours, currentState with
        | 3, Inactive -> Active
        | an, Active when an = 3 || an = 2 -> Active
        | _, _ -> Inactive
    coords, nextState

let runCycle (pocketState:CubeMap) : CubeMap =
    let dimensions = possibleDimensions pocketState
    let newStates = dimensions |> Seq.map (applyRules pocketState)
    newStates |> Seq.fold (fun cm (coord,state) -> cm |> Map.add coord state) pocketState

let rec run round pocketState =
    if round > 0 then
        let newState = runCycle pocketState
        run (round-1) newState
    else
        pocketState

parseInput sampleInput
|> run 6
|> Map.toSeq |> Seq.map snd |> Seq.filter ((=) Active) |> Seq.length