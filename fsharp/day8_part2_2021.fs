open System
open System.IO
open System.Text.RegularExpressions

let alphabetize (s:string) = s.ToCharArray() |> Array.sort |> String

let overlap (larger:string) (smaller:string) = smaller |> Seq.forall larger.Contains

let solve (input:string) =
    let lines = input.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
    let mutable total = 0L
    for line in lines do
        let parts =
            Regex.Matches(line, "[a-g]+")
            |> Seq.cast<Match>
            |> Seq.map (fun m -> alphabetize m.Value)
            |> Seq.toArray
        let digits = Array.zeroCreate<string> 10
        let working = parts.[0..9]
        let one = working |> Array.find (fun p -> p.Length = 2)
        let four = working |> Array.find (fun p -> p.Length = 4)
        let seven = working |> Array.find (fun p -> p.Length = 3)
        let eight = working |> Array.find (fun p -> p.Length = 7)
        digits.[1] <- one; digits.[4] <- four; digits.[7] <- seven; digits.[8] <- eight
        let filtered = working |> Array.filter (fun p -> p <> one && p <> four && p <> seven && p <> eight)
        let zeroThreeNine = filtered |> Array.filter (fun p -> overlap p one)
        let three = zeroThreeNine |> Array.find (fun p -> p.Length = 5)
        let nine = zeroThreeNine |> Array.find (fun p -> p <> three && overlap p four)
        let zero = zeroThreeNine |> Array.find (fun p -> p <> three && p <> nine)
        digits.[3] <- three; digits.[9] <- nine; digits.[0] <- zero
        let remaining = filtered |> Array.filter (fun p -> p <> zero && p <> three && p <> nine)
        let six = remaining |> Array.find (fun p -> p.Length = 6)
        let five = remaining |> Array.find (fun p -> p <> six && overlap nine p)
        let two = remaining |> Array.find (fun p -> p <> six && p <> five)
        digits.[6] <- six; digits.[5] <- five; digits.[2] <- two
        let output = parts.[10..13]
        let mutable value = 0L
        for o in output do
            let idx = digits |> Array.findIndex ((=) o)
            value <- value * 10L + int64 idx
        total <- total + value
    total

[<EntryPoint>]
let main _ =
    let input = File.ReadAllText("input.txt")
    let ans = solve input
    printfn "%d" ans
    0