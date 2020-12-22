#time
#r @"nuget: Unquote"
open Swensen.Unquote
open System
open System.IO

let sampleInput = $"{__SOURCE_DIRECTORY__}/sampleInput.txt"
let input = $"{__SOURCE_DIRECTORY__}/input.txt"

type Operation = Add | Multiply | NoOp

let calculateResult operation a b =
    match operation with
    | Add -> a + b
    | Multiply -> a * b
    | NoOp -> failwith "Invalid state while calculating Result!"

let parseEquationNodes (parts:string list)  =
    let rec execute stack (unprocessedParts:string list) =
        let setOpsAndContinue ops tail =
            let result, _ = stack |> List.head
            let stackTail = stack |> List.tail
            execute ((result, ops)::stackTail) tail
        match unprocessedParts with
        | head::tail -> 
            match head with
            | "+" -> setOpsAndContinue Add tail
            | "*" -> setOpsAndContinue Multiply tail
            | s when (Int64.TryParse(s) |> (fun (isInt, _) -> isInt)) -> 
                let result, operation = stack |> List.head
                let stackTail = stack |> List.tail
                let number = Convert.ToInt64 s
                let newResult = calculateResult operation result number
                execute ((newResult, NoOp)::stackTail) tail
            | s when s.StartsWith("(") ->
                let value = String (s.ToCharArray() |> Array.skip 1)
                execute ((1L,Multiply)::stack) (value::tail)
            | s when s.EndsWith(")") ->
                let result, operation = stack |> List.head

                let stackTail = stack |> List.tail

                let (nextRes, nextOperation) = stackTail |> List.head

                let hasParsed, number = Int64.TryParse (s.Substring(0,1))

                let newResult = 
                    if hasParsed then 
                        let newResult = calculateResult operation result number 
                        calculateResult nextOperation nextRes newResult
                    else
                        calculateResult nextOperation nextRes result

                // let nextNewRes = calculateResult nextOperation nextRes newResult
                let stackNextTail = stackTail |> List.tail

                let nonNumberStringPart = s.Substring(1)
                let tailToExecute =
                    if nonNumberStringPart.Length > 1 then // multi close i.e. 3))
                        ((nonNumberStringPart.Substring(1))::tail)
                    else
                        tail
                execute ((newResult, NoOp)::stackNextTail) tailToExecute
            | _ -> 
                failwith $"Oops - this state is not known... {head}" //stack unprocessedParts
        | [] -> stack |> List.head |> fst

    execute [(1L,Multiply)] parts

let parse (input:string) =
    input.Split(" ") |> Seq.toList |> parseEquationNodes

File.ReadAllLines(input)
|> Array.map parse
|> Seq.sum


test <@ parse "123" = 123L @>
test <@ parse "1 + 2" = 3L @>
test <@ parse "1 + 2 + 3" = 6L @>
test <@ parse "1 + 2 * 3" = 9L @>
test <@ parse "1 + (2 * 3)" = 7L @>
test <@ parse "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" = 13632L @>
test <@ parse "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" = 12240L @>
