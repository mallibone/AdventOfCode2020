#time
open System
open System.IO
open System.Collections.Generic

type DockingParameters = {
    CurrentAndMask : int64
    CurrentOrMask : int64
    Memory : Dictionary<int64,int64>
}

let init = {CurrentAndMask = 0L; CurrentOrMask = 0L; Memory = Dictionary<int64,int64>()}

let bitSequenceToNumber bits =
    bits |> Seq.fold (fun state bit -> (state <<< 1) + bit ) 0L

let convertToNumber (bits:string) =
    bits.ToCharArray() 
    |> Array.map ((fun c -> c.ToString()) >> Convert.ToInt64)
    |> bitSequenceToNumber

let sanitizeMask mask =
    let sanitizingMask = [ for i in 0 .. 35 do 1L ] |> bitSequenceToNumber
    (mask &&& sanitizingMask)

let parseOrMask (stringMask:string) =
    stringMask.Replace("X", "0") |> convertToNumber |> sanitizeMask

let parseAndMask (stringMask:string) =
    stringMask.Replace("X", "1") |> convertToNumber |> sanitizeMask

let parseMemoryInstruction (stringMemorySlot:string) =
    stringMemorySlot.Replace("mem[", "")
    |> fun s -> s.Replace("]", "")
    |> Convert.ToInt64

let parseMemoryValue andMask orMask value =
    ((value ||| orMask) &&& andMask)

let processInstruction state (instruction:string) =
    let instructionParameters = instruction.Split(" = ")
    let parseMemoryValue = parseMemoryValue state.CurrentAndMask state.CurrentOrMask
    match instructionParameters.[0] with
    | "mask" ->
        {state with CurrentAndMask = (parseAndMask instructionParameters.[1]); CurrentOrMask = (parseOrMask instructionParameters.[1])}
    | s when s.StartsWith("mem[") ->
        let memoryValue = (parseMemoryValue (instructionParameters.[1] |> Convert.ToInt64))
        let memorySlot = (parseMemoryInstruction s)

        if state.Memory.ContainsKey memorySlot then
            state.Memory.[memorySlot] <- memoryValue
        else
            state.Memory.Add(memorySlot, memoryValue)
        state
    | _ -> state

File.ReadAllLines("input.txt")
// File.ReadAllLines("sampleInput.txt")
|> Seq.fold processInstruction init
|> (fun state -> (state.Memory |> Seq.sumBy (fun x -> x.Value)))