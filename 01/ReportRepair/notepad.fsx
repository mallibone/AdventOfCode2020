open System
open System.IO

let readInput fileName =
    File.ReadAllLines(fileName)
    |> Array.map (fun (s) -> Convert.ToInt32(s))

let groupNumbers numbers =
    numbers 
    |> Array.filter(fun n -> not (numbers |> Array.filter(fun x -> x <> n && x+n=2020) |> Array.isEmpty))

let trioNumbers numbers =

    let otherNumbers toFilter = numbers |> Array.filter(fun x -> not(toFilter |> (Array.contains x)))

    let combinations = [|
                            for number1 in numbers do
                                for number2 in (otherNumbers [|number1|]) do
                                    for number3 in (otherNumbers [|number1; number2|]) do
                                        yield ([|number1; number2; number3|] |> Array.sort)
                        |]

    combinations |> Array.filter(fun n -> (Array.sum n) = 2020) |> Array.head

let correctedReport = readInput "input.txt"
                            // |> groupNumbers
                            |> trioNumbers
                            |> Array.fold (*) 1