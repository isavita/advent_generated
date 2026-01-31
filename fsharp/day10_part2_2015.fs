
open System
open System.IO
open System.Text

let nextSeq (s:string) =
    let sb = StringBuilder(s.Length * 2)
    let mutable i = 0
    while i < s.Length do
        let ch = s.[i]
        let mutable cnt = 1
        while i + cnt < s.Length && s.[i + cnt] = ch do
            cnt <- cnt + 1
        sb.Append(cnt).Append(ch) |> ignore
        i <- i + cnt
    sb.ToString()

[<EntryPoint>]
let main _ =
    let init = File.ReadAllText("input.txt").Trim()
    let result = Seq.fold (fun acc _ -> nextSeq acc) init (seq {1 .. 50})
    printfn "%d" result.Length
    0
