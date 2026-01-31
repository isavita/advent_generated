open System
open System.IO

let isRealRoom (line:string) =
    let bracket = line.IndexOf('[')
    if bracket = -1 then false, ""
    else
        let checksum = line.Substring(bracket+1,5)
        let namePart = line.Substring(0, bracket)
        let counts = Array.create 26 0
        for c in namePart do
            if Char.IsLetter c then
                counts.[int c - int 'a'] <- counts.[int c - int 'a'] + 1
        let expected =
            counts
            |> Array.mapi (fun i cnt -> cnt, char (i + int 'a'))
            |> Array.sortBy (fun (cnt,ch) -> -cnt, ch)
            |> Array.truncate 5
            |> Array.map snd
            |> String.Concat
        expected = checksum, checksum

let getSectorID (line:string) =
    let dash = line.LastIndexOf('-')
    let bracket = line.IndexOf('[')
    if dash = -1 || bracket = -1 then 0
    else Int32.Parse(line.Substring(dash+1, bracket-dash-1))

let decrypt (name:string) sectorID =
    let shift = sectorID % 26
    name.Replace('-', ' ')
    |> Seq.map (fun c ->
        if Char.IsLetter c then
            char (int 'a' + ((int c - int 'a' + shift) % 26))
        else c)
    |> String.Concat

[<EntryPoint>]
let main argv =
    let target = "northpole object"
    let answer =
        File.ReadLines("input.txt")
        |> Seq.choose (fun line ->
            let real,_ = isRealRoom line
            if real then
                let sector = getSectorID line
                let namePart = line.Substring(0, line.IndexOf('[')).Replace('-', ' ')
                let decrypted = decrypt namePart sector
                if decrypted.Contains(target) then Some sector else None
            else None)
        |> Seq.tryHead
    match answer with
    | Some v -> printfn "%d" v
    | None -> ()
    0