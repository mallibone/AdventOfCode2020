#time
open System
open System.IO
open System.Text.RegularExpressions

let parseInput rawInput =
    let rec parser input currentGroup parsedInput =
        match input with
        | [] -> parsedInput
        | [lastEntry] -> ($"{currentGroup}{lastEntry}")::parsedInput
        | entry::rest -> 
            match entry with
            | "" -> parser rest "" (currentGroup::parsedInput)
            | _ -> parser rest ($"{currentGroup}{entry}") parsedInput
    parser rawInput "" []

let parseInput2 rawInput =
    let rec parser input currentGroup parsedInput =
        match input with
        | [] -> parsedInput
        | [lastEntry] -> (lastEntry::currentGroup)::parsedInput
        | entry::rest -> 
            match entry with
            | "" -> parser rest [] (currentGroup::parsedInput)
            | _ -> parser rest (entry::currentGroup) parsedInput
    parser rawInput [] []

let allYesAnswers (parsedInput: string list list) =
    parsedInput
    |> List.map (fun groupAnswers ->
        (groupAnswers.Length, String.Join("", groupAnswers).ToCharArray() |> Array.toList |> List.sort |> List.groupBy (fun c -> c) ))
    |> List.map (fun (count, groupedAnswers) -> groupedAnswers |> List.filter (fun (_, values) -> values.Length = count) |> List.length)

// First quest
File.ReadAllLines "sampleInput.txt"
// File.ReadAllLines "input.txt"
|> Array.toList
|> parseInput
|> List.sumBy (fun e -> (e.ToCharArray() |> Array.distinct).Length)

// Second quest
// File.ReadAllLines "sampleInput.txt"
File.ReadAllLines "input.txt"
|> Array.toList
|> parseInput2
|> allYesAnswers
|> List.sum