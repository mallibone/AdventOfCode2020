#time
open System
open System.IO
open System.Text.RegularExpressions

let parseContainedBags containedBags =
    Regex.Matches(containedBags, "(\d+)\s(\w+)(\s?)(\w*)")
    |> Seq.map (fun m -> m.Value)
    |> Seq.toList
    |> List.map (fun m -> (Convert.ToInt32(Regex.Match(m, "^(\d+)").Value), Regex.Match(m, "(\w+)(\s?)(\w*)$").Value))

let getBagDefinitions (bagDefinition:string) =
    bagDefinition.Split(" bags contain ")
    |> (fun lines -> (lines.[0], (parseContainedBags lines.[1])))

let rec findPossibleContainers (bagToStow:string) (bagDefinitions:list<string * list<int * string>>) =
    // bagDefinitions |> Seq.filter (fun bd -> (bd |> Seq.exists (fun bi -> bi = bagToStow)))
    bagDefinitions 
    |> List.filter (fun (_, bd) -> (bd |> List.exists (fun (_, bi) -> bi = bagToStow)))
    |> List.map (fun (bag, _) -> bag)
    |> List.collect (fun bag -> [bag] |> List.append (findPossibleContainers bag bagDefinitions))
    |> List.distinct

let rec containingBags parentBag (bagDefinitions:list<string * list<int * string>>) =
    let childBags = bagDefinitions |> List.filter(fun (bag, _) -> bag = parentBag) |> List.collect (fun (_, containedBags) -> containedBags)
    match childBags with
    | [] -> 0
    | _ -> 
        (childBags |> List.sumBy (fun (count, name) -> count + count * (containingBags name bagDefinitions)))

// File.ReadLines("testInput.txt")
File.ReadLines("input.txt")
|> Seq.toList
|> List.map getBagDefinitions
|> findPossibleContainers "shiny gold"
|> List.length

// part 2
// File.ReadLines("test2Input.txt")
//File.ReadLines("testInput.txt")
 File.ReadLines("input.txt")
|> Seq.toList
|> List.map getBagDefinitions
|> containingBags "shiny gold"