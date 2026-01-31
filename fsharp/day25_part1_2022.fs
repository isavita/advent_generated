
open System.IO

let fromSnafu (s:string) =
    s |> Seq.fold (fun n c ->
        n * 5L +
        match c with
        | '=' -> -2L
        | '-' -> -1L
        | d   -> int64 (d - '0')) 0L

let toSnafu n =
    let rec loop n acc =
        if n = 0L then acc else
        match n % 5L with
        | 3L -> loop ((n + 5L) / 5L) ('='::acc)
        | 4L -> loop ((n + 5L) / 5L) ('-'::acc)
        | d  -> loop (n / 5L) (char (int '0' + int d)::acc)
    if n = 0L then "0" else System.String(loop n [] |> List.toArray)

[<EntryPoint>]
let main _ =
    File.ReadAllLines("input.txt")
    |> Array.sumBy fromSnafu
    |> toSnafu
    |> printfn "%s"
    0
