#time
// #r @"nuget: Unquote"
// open Swensen.Unquote
open System
open System.IO

let sampleInput = $"{__SOURCE_DIRECTORY__}/sampleInput.txt"
let input = $"{__SOURCE_DIRECTORY__}/input.txt"

let calc (input:string list) index operation = 
    let res = operation (Convert.ToInt64 input.[index-1]) (Convert.ToInt64 input.[index+1])
    let newList = 
        [
            yield! input |> List.take (index-1)
            res.ToString()
            yield! input |> List.skip (index+2)
        ]
    newList

let rec calculateString (input:string list) =
    let addIndex = input |> Seq.tryFindIndex (fun s -> s = "+")
    let multIndex = input |> Seq.tryFindIndex (fun s -> s = "*")
    match addIndex, multIndex with
    | Some index, _ ->
        calculateString (calc input index (+))
    | None, Some index -> 
        calculateString (calc input index (*))
    | None, None -> (List.head input)

let rec parse (input:string) =
    let lastParentesesIndex = input.LastIndexOf("(")
    if lastParentesesIndex > -1 then
        let parantesesContent = input.Substring(lastParentesesIndex+1)
        let lastClosingParantesesIndex = parantesesContent.IndexOf(")")
        let parentesesCalculations = parantesesContent.Substring(0,lastClosingParantesesIndex)
        let parentesesResult = calculateString (parentesesCalculations.Split(" ") |> Array.toList)
        let newInput =
            (input.Substring(0, lastParentesesIndex) 
                + parentesesResult.ToString() 
                + input.Substring(lastParentesesIndex + lastClosingParantesesIndex + 2))
        parse  newInput
    else
        Convert.ToInt64 (calculateString (input.Split(" ") |> Array.toList))

File.ReadAllLines input
|> Array.sumBy parse

// test <@ parse "123" = 123L @>
// test <@ parse "1 + (2 * 3) + (4 * (5 + 6))" = 51L @>
// test <@ parse "2 * 3 + (4 * 5)" = 46L @>
// test <@ parse "5 + (8 * 3 + 9 + 3 * 4 * 3)" = 1445L @>
// test <@ parse "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" = 669060L @>
// test <@ parse "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" = 23340L @>
