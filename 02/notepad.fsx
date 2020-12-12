open System.IO
open System

type Input =
    {   Min:int
        Max:int
        Character:string
        Password:string }

let lineParser (line:string) =
    let input = line.Split(":")
    let parsedVerifier = input.[0].Split(" ")
    let character = parsedVerifier.[1]
    let min = Convert.ToInt32((parsedVerifier.[0].Split("-").[0]))
    let max = Convert.ToInt32((parsedVerifier.[0].Split("-").[1]))
    let password = input.[1]
    {Min = min; Max = max; Character = character; Password = password}

let readInput filename =
    File.ReadAllLines(filename)
    |> Array.map lineParser

let verifyInput input =
    let originalLenght = input.Password.Length
    let lengthAfterReplacing = input.Password.Replace(input.Character, "").Length
    let occurences = originalLenght - lengthAfterReplacing
    (occurences >= input.Min && occurences <= input.Max)

let verifyInput2 input =
    let firstCharOccurence = input.Password.[input.Min].ToString() = input.Character
    let secondCharOccurence = input.Password.[input.Max].ToString() = input.Character

    firstCharOccurence <> secondCharOccurence

readInput "input.txt" |> Array.filter verifyInput2 |> Array.length