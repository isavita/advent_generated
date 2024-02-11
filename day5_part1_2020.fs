
module Day5

let input = System.IO.File.ReadAllLines "input.txt"

let decodeSeatID (boardingPass:string) =
    let rec binarySearch (min:int) (max:int) (chars:char list) =
        match chars with
        | [] -> min
        | 'F'::rest | 'L'::rest -> binarySearch min (min + (max - min) / 2) rest
        | 'B'::rest | 'R'::rest -> binarySearch (min + (max - min) / 2 + 1) max rest
        | _ -> failwith "Invalid character"

    let row = binarySearch 0 127 (List.ofSeq boardingPass.[0..6])
    let column = binarySearch 0 7 (List.ofSeq boardingPass.[7..9])
    row * 8 + column

let maxSeatID = input |> Array.map decodeSeatID |> Array.max

printfn "%d" maxSeatID
