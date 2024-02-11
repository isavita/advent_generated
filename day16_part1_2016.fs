
module DayN

open System.IO

let diskLength = 272

let readInitialState (filename: string) =
    File.ReadLines filename
    |> Seq.head

let generateData (initialState: string) (length: int) =
    let rec generateDataRec data =
        if String.length data >= length then data.[..(length-1)]
        else
            let b = System.Text.StringBuilder()
            for i in (String.length data - 1) .. -1 .. 0 do
                if data.[i] = '0' then b.Append '1' |> ignore
                else b.Append '0' |> ignore
            generateDataRec (data + "0" + b.ToString())
    generateDataRec initialState

let calculateChecksum (data: string) =
    let rec calculateChecksumRec data =
        if String.length data % 2 = 1 then data
        else
            let b = System.Text.StringBuilder()
            for i in 0 .. 2 .. (String.length data - 1) do
                if data.[i] = data.[i+1] then b.Append '1' |> ignore
                else b.Append '0' |> ignore
            calculateChecksumRec (b.ToString())
    calculateChecksumRec data

let initialState = readInitialState "input.txt"
let data = generateData initialState diskLength
let checksum = calculateChecksum data
printfn "Checksum: %s" checksum
