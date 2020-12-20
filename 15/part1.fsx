open System
open System.IO
open System.Collections.Generic

let playGame finalRound startingNumbers =
    let memory = Dictionary<int,int>()
    startingNumbers 
    |> Seq.take ((startingNumbers |> Seq.length)-1) 
    |> Seq.iteri (fun index number -> (memory.Add(number, index)))

    let rec playGame round lastValue =
        if round = finalRound then
            lastValue
        else 
            if memory.ContainsKey(lastValue) then
                let number = (round-1) - memory.[lastValue]
                memory.[lastValue] <- (round-1)
                playGame (round+1) number
            else
                memory.Add(lastValue, round-1)
                playGame (round+1) 0

    playGame (memory.Count+1) (startingNumbers |> Seq.last)

#time
// File.ReadAllLines("sampleInput.txt")
// let rounds = 2020 // part I
let rounds = 30000000 // part II
File.ReadAllLines("input.txt")
|> Array.map ((fun line -> line.Split(",")) >> Seq.map Convert.ToInt32 >> (playGame rounds))