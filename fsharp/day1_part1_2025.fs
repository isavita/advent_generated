
open System
open System.IO
[<EntryPoint>]
let main _ =
    File.ReadAllText("input.txt").Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.fold (fun (p, c) (s:string) ->
        let d, a = s.[0], int(s.Substring(1))
        let p' = (p + if d = 'R' then a else -a) % 100
        let p'' = if p' < 0 then p' + 100 else p'
        p'', if p'' = 0 then c + 1 else c) (50, 0)
    |> snd |> printfn "%d"
    0
