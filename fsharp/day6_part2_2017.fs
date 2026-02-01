
open System
open System.IO

[<EntryPoint>]
let main _ =
    let banks =
        File.ReadAllText "input.txt"
        |> fun s -> s.Split([|' ';'\n';'\r';'\t'|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int

    let seen = System.Collections.Generic.Dictionary<string,int>()
    let rec loop cycles (b:int[]) =
        let key = String.Join(",", b)
        match seen.TryGetValue key with
        | true, old ->
            printfn "%d" (cycles - old)
            0
        | _ ->
            seen.[key] <- cycles
            let maxi = b |> Array.mapi (fun i v -> i,v) |> Array.maxBy snd |> fst
            let blocks = b.[maxi]
            b.[maxi] <- 0
            for i in 1..blocks do
                b.[(maxi + i) % b.Length] <- b.[(maxi + i) % b.Length] + 1
            loop (cycles + 1) b
    loop 0 banks
