open System
open System.IO

type Orientation =
| North 
| East
| South
| West

type State = {
    WaypointNorth: int
    WaypointEast: int
    MoveNorth : int
    MoveEast : int
}

type Command =
| RotateLeft
| RotateRight
| MoveForward
| ChangeVerticalWaypoint
| ChangeHorizontalWaypoint

type Instruction = {
    Command : Command
    Value : int
}

let convertInstruction (instructionText:string) =

    let commandText = (instructionText.ToCharArray() |> Array.head).ToString()
    let value = Convert.ToInt32(String(instructionText.ToCharArray() |> Array.skip 1))

    match commandText with
        | "F" -> MoveForward, value
        | "R" -> RotateRight, value
        | "L" -> RotateLeft, value
        | "N" -> ChangeVerticalWaypoint, value
        | "E" -> ChangeHorizontalWaypoint, value
        | "S" -> ChangeVerticalWaypoint, -value
        | "W" -> ChangeHorizontalWaypoint, -value
        | _ -> 
            raise (ArgumentException(sprintf "Unkown command character %s" commandText))

let parseInput fileName =
    File.ReadAllLines fileName
    |> Array.map convertInstruction

let rotateWaypoint (north,east) rotation value =
    match rotation, value with
    | RotateRight, 0 -> (north,east)
    | RotateRight, 90 -> (-east,north)
    | RotateRight, 180 -> (-north,-east)
    | RotateRight, 270 -> (east,-north)
    | RotateRight, 360 -> (north,east)
    | RotateLeft, 0 -> (north,east)
    | RotateLeft, 90 -> (east, -north)
    | RotateLeft, 180 -> (-north,-east)
    | RotateLeft, 270 -> (-east,north)
    | RotateLeft, 360 -> (north,east)
    | _, _ -> raise (ArgumentException(sprintf "Unkown values %A %d" rotation value))

let moveShip currentState (command, (value:int)) =
    match command with
    | RotateRight -> 
        let waypointNorth, waypointEast = rotateWaypoint (currentState.WaypointNorth, currentState.WaypointEast) RotateRight value
        {currentState with WaypointEast =  waypointEast; WaypointNorth = waypointNorth}
    | RotateLeft -> 
        let waypointNorth, waypointEast = rotateWaypoint (currentState.WaypointNorth, currentState.WaypointEast) RotateLeft value
        {currentState with WaypointEast =  waypointEast; WaypointNorth = waypointNorth}
    | MoveForward ->
        {currentState with MoveNorth = currentState.MoveNorth + (value*currentState.WaypointNorth); MoveEast = currentState.MoveEast + (value*currentState.WaypointEast)}
    | ChangeVerticalWaypoint ->
        {currentState with WaypointNorth = currentState.WaypointNorth + value}
    | ChangeHorizontalWaypoint ->
        {currentState with WaypointEast = currentState.WaypointEast + value}

let logMoveShip currentState instruction =
    let state = moveShip currentState instruction
    state

let manhattenDistance (currentState:State) =
    let distance = Math.Abs(currentState.MoveNorth) + Math.Abs(currentState.MoveEast)
    printfn "%s %d / %s %d" (if currentState.MoveNorth > 0 then "North" else "South") (Math.Abs(currentState.MoveNorth)) (if currentState.MoveEast > 0 then "East" else "West") (Math.Abs(currentState.MoveEast))
    distance

#time
parseInput "input.txt"
// parseInput "sampleInput.txt"
|> Array.fold logMoveShip {WaypointEast = 10; WaypointNorth = 1; MoveNorth = 0; MoveEast = 0}
|> manhattenDistance
