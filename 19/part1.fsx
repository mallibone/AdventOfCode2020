#time
// #r @"nuget: Unquote"
// open Swensen.Unquote
open System
open System.IO
open System.Text.RegularExpressions

let sampleInput = $"{__SOURCE_DIRECTORY__}/sampleInput.txt"
let input = $"{__SOURCE_DIRECTORY__}/input.txt"

type Rule =
| References of int list
| ReferenceOptions of Rule list
| Character of string

let parse (input:string) =
    let splitInput = input.Split($"{System.Environment.NewLine}{System.Environment.NewLine}")
    let rules = splitInput.[0]
    let messages = splitInput.[1]
    (rules.Split($"{System.Environment.NewLine}"), messages.Split($"{System.Environment.NewLine}"))

let rec parseSubRules (input:string) : Rule =
    match input with
    | i when i.Contains("|") -> 
        ReferenceOptions (i.Split(" | ") |> Seq.toList |> List.map parseSubRules)
    | i when i.Contains(" ") -> References (i.Split(" ") |> Seq.toList |> List.map Convert.ToInt32)
    | i when Regex.IsMatch(i, "\d+") -> References [Convert.ToInt32(i)]
    | i when Regex.IsMatch(i, "\D+") -> Character (i.Replace("\"", ""))
    | _ -> failwith $"Unkown input {input}"

let parseRules (rules:string seq) =
    let orderedRules = 
        rules 
        |> Seq.map((fun rule -> rule.Split(": ")) >> (fun splitRule -> (Convert.ToInt32 splitRule.[0], parseSubRules splitRule.[1])))
        |> Seq.sortBy fst
        |> Seq.map (fun (_, values) -> values)
        |> Seq.toList
    orderedRules


let combineSequences (sequences : string list list) : string list =
    let combineValueToList values value : string list=
        values |> List.map (sprintf "%s%s" value)

    let rec exec (inputSequences : string list list) (result:string list) =
        match inputSequences with
        | [] -> result
        | [lastElement] ->
            result 
            |> List.collect (combineValueToList lastElement)
        | head::tail ->
            let nextResult =
                result 
                |> List.collect (combineValueToList head)
            exec tail nextResult
    
    exec (sequences |>  List.tail) (sequences |> List.head)

let calculateValidPatterns (rules : Rule list ) : string list =
    let rec exec (rule : Rule) : string list=
        match rule with
        | References refs ->
            refs
            |> List.map (fun ref -> exec (rules.[ref]))
            |> combineSequences
        | ReferenceOptions refOptions ->
            refOptions 
            |> List.map exec
            |> List.collect id
        | Character character -> [character]
    exec rules.[0]


let inputRules, messages =
    File.ReadAllText input 
    |> parse

let rules = parseRules inputRules

let validMessagePatterns = calculateValidPatterns rules

messages 
|> Seq.filter(fun msg -> validMessagePatterns |> Seq.contains msg)
|> Seq.length