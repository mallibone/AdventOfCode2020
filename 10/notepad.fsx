#time
open System
open System.IO

// File.ReadAllLines "testInput.txt"
File.ReadAllLines "input.txt"
|> Seq.map Convert.ToInt32
|> Seq.sort
|> Seq.windowed 2
|> Seq.map (fun [|a;b|] -> b-a)
|> Seq.groupBy (fun a -> a)
|> Seq.map(fun(n , a) -> (n, (a |> Seq.length)+1))
|> Seq.map(fun(_ , a) -> a)
|> Seq.reduce (*)