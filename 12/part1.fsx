open System
open System.IO

type Orientation =
| North 
| East
| South
| West

type State = {
    Heading : Orientation
    MoveNorth : int
    MoveEast : int
}

type Command =
| RotateLeft
| RotateRight
| MoveInDirection
| MoveInOrientation of Orientation

type Instruction = {
    Command : Command
    Value : int
}

let convertInstruction (instructionText:string) =

    let commandText = (instructionText.ToCharArray() |> Array.head).ToString()

    let command = match commandText with
                    | "F" -> MoveInDirection
                    | "R" -> RotateRight
                    | "L" -> RotateLeft
                    | "N" -> MoveInOrientation North
                    | "E" -> MoveInOrientation East
                    | "S" -> MoveInOrientation South
                    | "W" -> MoveInOrientation West
                    | _ -> raise (ArgumentException(sprintf "Unkown command character %s" commandText))
    let value = Convert.ToInt32(String(instructionText.ToCharArray() |> Array.skip 1))
    (command, value)

let parseInput fileName =
    File.ReadAllLines fileName
    |> Array.map convertInstruction

let getOrientationFromHeading heading =
    if heading = 0 then North
    elif heading = 90 then East
    elif heading = 180 then South
    else West
    
let getHeadingFromOrientation orientation =
    match orientation with
    | North -> 0
    | East -> 90
    | South -> 180
    | West -> 270

let rotateHeadingBy orientation heading =
    let shipHeading = getHeadingFromOrientation orientation
    let newHeading = (shipHeading + heading) % 360
    if newHeading < 0 then 360 + newHeading else newHeading

let moveInHeading currentState heading value = 
    match heading with
    | East -> {currentState with MoveEast = (currentState.MoveEast + value)} 
    | West -> {currentState with MoveEast = (currentState.MoveEast - value)} 
    | North -> {currentState with MoveNorth = (currentState.MoveNorth + value)} 
    | South -> {currentState with MoveNorth = (currentState.MoveNorth - value)}

let moveShip currentState (command, (value:int)) =
    match command with
    | RotateRight -> 
        {currentState with Heading = getOrientationFromHeading (rotateHeadingBy currentState.Heading value)}
    | RotateLeft -> 
        ({currentState with Heading = getOrientationFromHeading (rotateHeadingBy currentState.Heading (-value))})
    | MoveInDirection ->
        (moveInHeading currentState currentState.Heading value)
    | MoveInOrientation heading ->
        moveInHeading currentState heading value

let manhattenDistance (currentState:State) =
    let distance = Math.Abs(currentState.MoveNorth) + Math.Abs(currentState.MoveEast)
    printfn "%s %d / %s %d" (if currentState.MoveNorth > 0 then "North" else "South") (Math.Abs(currentState.MoveNorth)) (if currentState.MoveEast > 0 then "East" else "West") (Math.Abs(currentState.MoveEast))
    distance

#time
// parseInput "input.txt"
parseInput "sampleInput.txt"
|> Array.fold moveShip {Heading = East; MoveNorth = 0; MoveEast = 0}
|> manhattenDistance
