
open System
open System.IO
open System.Text.RegularExpressions

let target = [|3;7;2;3;0;0;5;3;2;1|]
let attrIdx = dict [
    ("children",0); ("cats",1); ("samoyeds",2); ("pomeranians",3);
    ("akitas",4); ("vizslas",5); ("goldfish",6); ("trees",7);
    ("cars",8); ("perfumes",9) ]

let pattern = Regex(@"Sue (\d+): (\w+): (\d+), (\w+): (\d+), (\w+): (\d+)", RegexOptions.Compiled)

let matches (line:string) =
    let m = pattern.Match line
    if m.Success then
        let sue = int m.Groups.[1].Value
        let attrs = [
            (m.Groups.[2].Value, int m.Groups.[3].Value)
            (m.Groups.[4].Value, int m.Groups.[5].Value)
            (m.Groups.[6].Value, int m.Groups.[7].Value) ]
        let ok =
            attrs |> List.forall (fun (a, v) ->
                match attrIdx.TryGetValue a with
                | true, i ->
                    if i = 1 || i = 7 then v > target.[i]
                    elif i = 3 || i = 6 then v < target.[i]
                    else v = target.[i]
                | _ -> true)
        if ok then Some sue else None
    else None

[<EntryPoint>]
let main _ =
    let result =
        File.ReadLines "input.txt"
        |> Seq.choose matches
        |> Seq.tryHead
    match result with
    | Some sue -> printfn "%d" sue; 0
    | None -> eprintfn "No matching Sue found."; 1
