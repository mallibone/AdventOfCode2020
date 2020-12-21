#time
open System
open System.IO

type Range = { Min : int; Max : int }

type Field = { Name : string; ValidRanges : Range list }

type Input = {Fields : Field list; MyTicket : int list; NearbyTickets : int list list}

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
    |> Seq.toList

let parseTicket (ticketInput:string) =
    ticketInput.Split($"{System.Environment.NewLine}")
    |> Seq.skip 1
    |> Seq.map (fun (row:string) -> (row.Split(",") |> Seq.map Convert.ToInt32 |> Seq.toList ))
    |> Seq.toList

let parseInput (input:string) =
    let groups = input.Split($"{System.Environment.NewLine}{System.Environment.NewLine}")
    let fields = parseFields groups.[0]
    let myTicket = parseTicket groups.[1] |> List.head
    let nearbyTickets = parseTicket groups.[2]
    {Fields = fields; MyTicket = myTicket; NearbyTickets = nearbyTickets}

let applyRule ranges field =
    ranges |> Seq.exists(fun range -> range.Min <= field && field <= range.Max)

let filterInvalidTickets fields (nearbyTickets:int list list) =
    let ranges = fields |> List.collect (fun range -> range.ValidRanges)
    nearbyTickets
    |> List.filter (fun ticket -> ticket |> Seq.forall(applyRule ranges) )

let findRule (rules:Field list) tickets index =
    let ticketFields = tickets |> List.map (List.item index)
    let applicableRules =
        rules
        |> List.filter(fun rule -> ticketFields |> Seq.forall(applyRule rule.ValidRanges))
    match applicableRules with
    | [rule] -> Some (rule, index)
    | [] -> failwith "Hoppla! No rule was found..."
    | _ -> None
    

let assignRowsToFields indexes (fields:Field list) nearbyTickets = 
    let rec assign fieldIndexes (rulesToAssign:Field list) =
        match fieldIndexes with
        | [] -> []
        | _ ->
            let (rule, index) =
                fieldIndexes
                |> List.pick (findRule rulesToAssign nearbyTickets)
            (rule.Name, index) :: (assign (fieldIndexes |> List.except [index]) (rulesToAssign |> List.except [rule]))

    assign indexes fields

let parsedInput =
    File.ReadAllText "input.txt"
    |> parseInput

let validTickets =
    filterInvalidTickets parsedInput.Fields parsedInput.NearbyTickets
let indexes = 
    parsedInput.MyTicket |> Seq.indexed |> Seq.map fst |> Seq.toList

let assignedFields = assignRowsToFields indexes parsedInput.Fields validTickets
let result = 
    assignedFields 
    |> Seq.filter(fun (fieldName, _) -> fieldName.StartsWith("departure")) 
    |> Seq.toList
    |> Seq.map (fun (_, index) -> Convert.ToInt64 parsedInput.MyTicket.[index])
    |> Seq.toList
    |> Seq.reduce (*)
printfn "The product of the fields is: %d" result
// |> (fun (fields, myTicket, nearbyTickets) -> fields, myTicket, )
// |> (fun (fields, myTicket, nearbyTickets) -> fields, myTicket, assignRowsToFields fields nearbyTickets)
// |> (fun (_,_,otherTickets) -> otherTickets |> Seq.collect (Seq.indexed)) |> Seq.groupBy (fun (index, _) -> index)