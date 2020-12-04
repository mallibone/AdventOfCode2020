#time
open System
open System.IO
open System.Text.RegularExpressions

let openFile filename =
    File.ReadAllLines filename

let parseRawPassportData rawData =
    let rec parser data currentResult parsedResult =
        match data with
        | [] -> currentResult::parsedResult
        | [item] -> ((sprintf "%s %s" currentResult item).Trim()::parsedResult)
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

let isValidPassport2 (passportData:((string * string) list)) =
    (passportData
    |> List.map (fun field ->
        match field with
        | ("byr", value) -> (Regex.Match(value, "^\d{4}$").Success && Convert.ToInt32(value) <= 2002 && (Convert.ToInt32(value) >= 1920))
        | ("iyr", value) -> (Regex.Match(value, "^\d{4}$").Success && Convert.ToInt32(value) <= 2020 && Convert.ToInt32(value) >= 2010)
        | ("eyr", value) -> (Regex.Match(value, "^\d{4}$").Success && Convert.ToInt32(value) <= 2030 && Convert.ToInt32(value) >= 2020)
        | ("hgt", value) when (Regex.Match(value, "\d{3}(cm)").Success) -> 
            Convert.ToInt32(Regex.Match(value, "\d{3}").Value) <= 193 && Convert.ToInt32(Regex.Match(value, "\d{3}").Value) >= 150
        | ("hgt", value) when (Regex.Match(value, "\d{2}(in)").Success) -> 
            Convert.ToInt32(Regex.Match(value, "\d{2}").Value) <= 76 && Convert.ToInt32(Regex.Match(value, "\d{2}").Value) >= 59
        | ("hcl", value) -> Regex.Match(value, "#[a-f0-9]{6}$").Success
        | ("ecl", value) -> Regex.Match(value, "(amb|blu|brn|gry|grn|hzl|oth)").Success
        | ("pid", value) -> Regex.Match(value, "^\d{9}$").Success
        | ("cid", _) -> true
        | (_,_) -> false)
    |> List.forall ((=) true)) && isValidPassport passportData
    
// openFile "testinput.txt" 
openFile "input.txt" 
|> Array.toList 
|> parseRawPassportData 
|> List.rev 
|> List.map parsePassportData 
// |> List.filter isValidPassport // part 1
|> List.filter isValidPassport2 // part 2
|> List.length