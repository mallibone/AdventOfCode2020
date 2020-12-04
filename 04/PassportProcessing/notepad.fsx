#time
open System
open System.IO

let openFile filename =
    File.ReadAllLines filename

let parseRawPassportData rawData =
    let rec parser data currentResult parsedResult =
        match data with
        | [] -> currentResult::parsedResult
        | [item] -> ((sprintf "%s %s" currentResult item)::parsedResult)
        | head::tail -> 
            match head with
            | "" -> parser tail "" ((currentResult.Trim())::parsedResult)
            | _ -> parser tail (sprintf "%s %s" currentResult head) parsedResult
    
    parser rawData "" []

let parsePassportData (passportData:string) =
    let items = passportData.Split(" ")
    items |> Array.map(fun item -> 
                                    let keyValue = item.Split(":")
                                    (keyValue.[0], keyValue.[1]))
          |> Array.toList

let isValidPassport (passportData:((string * string) list)) =
    let requiredFiels = ["byr";"iyr";"eyr";"hgt";"hcl";"ecl";"pid"]//;"cid"]
    let keys = String.Concat((passportData |> List.map fst))
    requiredFiels |> List.map keys.Contains |> List.forall ((=) true)
    
// openFile "testinput.txt" 
openFile "input.txt" 
|> Array.toList |> parseRawPassportData |> List.rev |> List.map parsePassportData 
|> List.filter isValidPassport 
|> List.length