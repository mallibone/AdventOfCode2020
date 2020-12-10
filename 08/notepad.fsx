#time
open System
open System.IO

type Instruction = {
    Operation:string
    Argument:int
}

type Result =
| Ok of int
| InfiniteLoop of int
| Error

let parseInput (input:string) =
    let values = input.Split(" ")
    {Operation = values.[0]; Argument = Convert.ToInt32(values.[1])}

let rec findInfiniteLoop visitedIndex index acc (instructions:Instruction array) =
    if visitedIndex |> List.contains index then
        InfiniteLoop acc
    elif index = instructions.Length then
        Ok acc
    else
        match instructions.[index] with
        | {Operation = "nop"} -> findInfiniteLoop (index::visitedIndex) (index+1) acc instructions
        | {Operation = "jmp"} -> findInfiniteLoop (index::visitedIndex) (index+instructions.[index].Argument) acc instructions
        | {Operation = "acc"} -> findInfiniteLoop (index::visitedIndex) (index+1) (acc+instructions.[index].Argument) instructions
        | _ ->
            printfn "Invalid value"
            Error

let rec fixProgram instructionIndex (instructions:Instruction array) =
    let swapOperation i newOp =
        instructions.[i] <- {Operation = newOp; Argument = instructions.[instructionIndex].Argument}

    match instructions.[instructionIndex] with
    | {Operation = "nop"} ->
        swapOperation instructionIndex "jmp"
        match findInfiniteLoop [] 0 0 instructions with
        | Ok value -> value
        | InfiniteLoop _ -> 
            swapOperation instructionIndex "nop"
            fixProgram (instructionIndex+1) instructions
        | Error -> 0
    | {Operation = "jmp"} ->
        swapOperation instructionIndex "nop"
        match findInfiniteLoop [] 0 0 instructions with
        | Ok value -> value
        | InfiniteLoop _ -> 
            swapOperation instructionIndex "jmp"
            fixProgram (instructionIndex+1) instructions
        | Error -> 0
    | _ -> fixProgram (instructionIndex+1) instructions

// File.ReadAllLines "testinput.txt"
File.ReadAllLines "input.txt"
|> Array.map parseInput
|> (findInfiniteLoop [] 0 0)