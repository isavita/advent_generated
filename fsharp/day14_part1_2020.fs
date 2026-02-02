
open System
open System.IO
open System.Text.RegularExpressions

let applyMask (value: int64) (mask: string) =
    let result = ref 0L
    for i in 0 .. 35 do
        let bitValue = 1L <<< (35 - i)
        match mask.[i] with
        | '1' -> result := !result ||| bitValue
        | 'X' -> result := !result ||| (value &&& bitValue)
        | _ -> ()
    !result

let main () =
    let mem = System.Collections.Generic.Dictionary<int64, int64>()
    let reMem = Regex(@"mem\[(\d+)] = (\d+)")
    use sr = File.OpenText("input.txt")
    let mutable mask = ""
    let mutable line = sr.ReadLine()
    while not (isNull line) do
        if line.StartsWith("mask = ") then
            mask <- line.Substring(7)
        else
            let m = reMem.Match(line)
            if m.Success then
                let address = Int64.Parse(m.Groups.[1].Value)
                let value = Int64.Parse(m.Groups.[2].Value)
                mem.[address] <- applyMask value mask
        line <- sr.ReadLine()
    let sum = mem.Values |> Seq.sum
    printfn "%d" sum

main ()
