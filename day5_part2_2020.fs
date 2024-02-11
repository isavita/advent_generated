
module Day5

let input = System.IO.File.ReadAllLines "input.txt"

let decodeSeatID (boardingPass: string) =
    let rec binarySearch (min: int, max: int) (chars: char list) =
        match chars with
        | [] -> min
        | 'F'::rest | 'L'::rest -> binarySearch (min, min + (max - min) / 2) rest
        | 'B'::rest | 'R'::rest -> binarySearch (min + (max - min + 1) / 2, max) rest
        | _ -> failwith "Invalid input"
    
    let row = binarySearch (0, 127) (List.ofSeq boardingPass.[0..6])
    let column = binarySearch (0, 7) (List.ofSeq boardingPass.[7..9])
    
    row * 8 + column

let seatIDs = input |> Array.map decodeSeatID
let maxSeatID = seatIDs |> Array.max
let missingSeatID = [1..1023] |> List.filter (fun x -> not (Array.contains x seatIDs))

printfn "%d" maxSeatID
printfn "%A" missingSeatID
