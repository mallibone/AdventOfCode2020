open System
open System.IO

type Range = { Min : int; Max : int }

type Field = { Name : string; ValidRanges : Range list }

let parseFields (fieldInput:string) = 
    let parseFieldRow (rowInput:string) =
        let inputs = rowInput.Split(":")
        let ranges = (inputs.[1]).Trim().Split(" or ")
                            |> Seq.map( fun ri -> 
                                                    let rangeParameterInput = ri.Split("-")
                                                    {Min = Convert.ToInt32 rangeParameterInput.[0]; 
                                                        Max = Convert.ToInt32 rangeParameterInput.[1]})
                            |> Seq.toList
        { Name = inputs.[0]; ValidRanges = ranges}
    fieldInput.Split($"{System.Environment.NewLine}")
    |> Seq.map parseFieldRow

let parseTicket (ticketInput:string) = 
    ticketInput.Split($"{System.Environment.NewLine}") 
    |> Seq.skip 1
    |> Seq.map (fun (row:string) -> (row.Split(",") |> Seq.map Convert.ToInt32))
    |> Seq.toList

let parseInput (input:string) =
    let groups = input.Split($"{System.Environment.NewLine}{System.Environment.NewLine}")
    let fields = parseFields groups.[0]
    let myTicket = parseTicket groups.[1]
    let nearbyTickets = parseTicket groups.[2]
    fields, myTicket, nearbyTickets

let findInvalidFields (fields, _, (nearbyTickets:int seq list)) = 
    let ranges = fields |> Seq.collect (fun range -> range.ValidRanges)
    nearbyTickets
    |> Seq.collect ( fun a -> a )
    |> Seq.filter (fun field -> ranges |> Seq.forall (fun range -> field < range.Min || field > range.Max))


#time
// File.ReadAllText "sampleInput.txt"
File.ReadAllText "input.txt"
|> parseInput
|> findInvalidFields
|> Seq.sum