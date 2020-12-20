open System
open System.IO
open System.Collections.Generic

type DockingParameters = {
    CurrentMasks : int64 list
    Memory : Dictionary<int64,int64>
}

let bitSequenceToNumber bits =
    bits |> Seq.fold (fun state bit -> (state <<< 1) + bit ) 0L

let convertToNumber (bits:string) =
    bits.ToCharArray()
    |> Array.map ((fun c -> c.ToString()) >> Convert.ToInt64)
    |> bitSequenceToNumber

let sanitizeMask mask =
    let sanitizingMask = [ for i in 0 .. 35 do 1L ] |> bitSequenceToNumber
    (mask &&& sanitizingMask)

let parseMask (maskValue:string) =
    let getIndexes (maskValue:string) =
        maskValue.ToCharArray()
        |> Seq.mapi( fun i c -> if c = 'X' then i else -1)
        |> Seq.filter ((<=) 0)
        |> Seq.toList

    let floatingCount = (maskValue.ToCharArray() |> Array.sumBy (fun c -> if c = 'X' then 1 else 0))
    // let floatingMask = 1L <<< (floatingCount-1)
    let variations = 1L <<< floatingCount
    let binaryTable = [0L .. (variations - 1L)]
    let indexes = getIndexes maskValue
    binaryTable
    |> List.map (fun binaryValue ->
        let newMask = maskValue.ToCharArray()
        // let newMask = maskValue.Replace("X","1").ToCharArray()
        // indexes.Length, (floatingCount-1)
        for i in (indexes.Length-1) .. -1 .. 0 do
            newMask.[indexes.[(i)]] <- ((binaryValue>>>i) &&& 1L).ToString().[0]

        (String newMask) |> convertToNumber |> sanitizeMask
        )

let parseMemoryInstruction (stringMemorySlot:string) =
    stringMemorySlot.Replace("mem[", "")
    |> fun s -> s.Replace("]", "")
    |> Convert.ToInt64

let processInstruction state (instruction:string) =
    let instructionParameters = instruction.Split(" = ")
//    let parseMemoryValue = parseMemoryValue state.CurrentAndMask state.CurrentOrMask
    match instructionParameters.[0] with
    | "mask" ->
        {state with CurrentMasks = (parseMask instructionParameters.[1])}
    | s when s.StartsWith("mem[") ->
        let memoryValue = (instructionParameters.[1] |> Convert.ToInt64)
        let instructionSlot = (parseMemoryInstruction s)
        let memorySlots =
            let largestMask = (~~~(state.CurrentMasks |> List.max))
            state.CurrentMasks
            |> List.map (fun m -> (m ||| ((instructionSlot&&&largestMask) |> sanitizeMask)))
            |> List.sort
            |> List.distinct
        // printfn "%A" memorySlots
        memorySlots
        |> List.iter (fun memorySlot ->
            if state.Memory.ContainsKey memorySlot then
                state.Memory.[memorySlot] <- memoryValue
            else
                state.Memory.Add(memorySlot, memoryValue))
        state
    | _ -> state

#time
let init = { CurrentMasks = [0L]; Memory = Dictionary<int64,int64>() }

File.ReadAllLines "input.txt"
|> Seq.fold processInstruction init
|> (fun state -> (state.Memory |> Seq.sumBy (fun x -> x.Value)))