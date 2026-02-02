
open System
open System.Collections.Generic
open System.IO

let tableSize = 16384
let maskLen = 36

let hashFunction (key: uint64) = uint32 (key % (uint64 tableSize))

let mutable hashTable = Array.init tableSize (fun _ -> new Dictionary<uint64, uint64>())

let mutable currentMask = ""

let rec storeAddresses (bitIndex: int) (currentAddress: uint64) (value: uint64) =
    if bitIndex < 0 then
        let index = hashFunction currentAddress
        if hashTable.[int index].ContainsKey currentAddress then
            hashTable.[int index].[currentAddress] <- value
        else
            hashTable.[int index].[currentAddress] <- value
    else
        let maskPos = maskLen - 1 - bitIndex
        let bitMask = 1UL <<< bitIndex
        match currentMask.[maskPos] with
        | '0' -> storeAddresses (bitIndex - 1) currentAddress value
        | '1' -> storeAddresses (bitIndex - 1) (currentAddress ||| bitMask) value
        | 'X' ->
            storeAddresses (bitIndex - 1) (currentAddress &&& (~~~bitMask)) value
            storeAddresses (bitIndex - 1) (currentAddress ||| bitMask) value
        | _ -> failwith "Invalid mask character"

let sumValues () =
    let totalSum = ref 0UL
    for i = 0 to tableSize - 1 do
        for kvp in hashTable.[i] do
            totalSum := !totalSum + kvp.Value
    !totalSum

let main () =
    try
        let lines = File.ReadAllLines("input.txt")
        for line in lines do
            if line.StartsWith("mask = ") then
                currentMask <- line.Substring(7).Trim()
            elif line.StartsWith("mem") then
                let parts = line.Split('=')
                let address = uint64 (parts.[0].Trim().Substring(4).Trim('[', ']'))
                let value = uint64 (parts.[1].Trim())
                storeAddresses (maskLen - 1) address value
        printfn "%A" (sumValues ())
    with
    | ex -> printfn "An error occurred: %s" ex.Message

main ()
