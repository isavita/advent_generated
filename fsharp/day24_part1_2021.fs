
open System
open System.IO

[<EntryPoint>]
let main argv =
    let file = "input.txt"
    if not (File.Exists file) then
        printfn "Error opening file"
        0
    else
        let lines = File.ReadAllLines file
        let l = Array.zeroCreate 14
        let k = Array.zeroCreate 14
        let m = Array.zeroCreate 14

        for i = 0 to lines.Length - 1 do
            match i % 18 with
            | 4 ->
                let v = Int32.Parse((lines.[i].Split ' ') |> Array.last)
                l.[i / 18] <- v
            | 5 ->
                let v = Int32.Parse((lines.[i].Split ' ') |> Array.last)
                k.[i / 18] <- v
            | 15 ->
                let v = Int32.Parse((lines.[i].Split ' ') |> Array.last)
                m.[i / 18] <- v
            | _ -> ()

        let paired = Array.zeroCreate 14
        let diff   = Array.zeroCreate 14
        let stack  = Array.zeroCreate 14
        let mutable stackSize = 0

        for i = 0 to 13 do
            if l.[i] = 1 then
                stack.[stackSize] <- i
                stackSize <- stackSize + 1
            elif l.[i] = 26 then
                stackSize <- stackSize - 1
                let j = stack.[stackSize]
                paired.[j] <- i
                diff.[j] <- m.[j] + k.[i]

        let maxDigits = Array.zeroCreate 14

        for i = 0 to 13 do
            if paired.[i] = 0 && diff.[i] = 0 then ()
            else
                let mutable vmax = 9
                let di = diff.[i]
                while vmax + di > 9 do
                    vmax <- vmax - 1
                maxDigits.[i] <- vmax
                maxDigits.[paired.[i]] <- vmax + di

        let result = String.Join("", maxDigits |> Array.map string)
        printfn "%s" result
        0
