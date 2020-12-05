#time
open System
open System.IO
open System.Text
open System.Text.RegularExpressions

let readInput filename =
    File.ReadAllLines filename

let extractRowAndSeat textRow =
    let row = Regex.Match(textRow, "^(F|B){7}").Value
    let aisle = Regex.Match(textRow, "(L|R){3}$").Value
    (row, aisle)

let convertToSeatNumber ((row:string), (seat:string)) =
    let converter identifier letters =
        letters
        |> Array.map (fun c -> if c = identifier then 0b0 else 0b1)
        |> Array.fold (fun state digit -> (((state)<<<1) + digit)) 0
    let rowNumber = row.ToCharArray() |> converter 'F'
    let seatNumber = seat.ToCharArray() |> converter 'L'
    (rowNumber, seatNumber)
    
let getSeatId (row, seat) =
    row * 8 + seat

let rowsWithEmptySeats seats =
    let filterFirstAndLastRow groupedRows =
        groupedRows
        |> Array.skip 1
        |> Array.take (groupedRows.Length - 2)
    seats
    |> Array.sortBy fst
    |> Array.groupBy fst
    |> filterFirstAndLastRow
    |> Array.filter (fun (_,values) -> values.Length < 8)

let getEmptySeats (row, seats) =
    let isSeatTaken aisleNumber =
        seats
        |> Array.exists (fun (_, aisle) -> aisle = aisleNumber)
    let aisleNumbers = [|0 .. 7|]
    aisleNumbers
    |> Array.filter (isSeatTaken >> not)
    |> Array.map (fun n -> (row, n))

// readInput "testinput.txt"
// part 1
readInput "input.txt"
|> Array.map (extractRowAndSeat >> convertToSeatNumber >> getSeatId)
|> Array.sort
|> Array.max

// part 2
readInput "input.txt"
|> Array.map (extractRowAndSeat >> convertToSeatNumber)
|> rowsWithEmptySeats
|> Array.collect (getEmptySeats)
|> Array.map getSeatId
