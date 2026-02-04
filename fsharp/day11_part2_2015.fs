
open System
open System.IO

let increment (p:string) =
    let a = p.ToCharArray()
    let rec go i =
        if i >= 0 then
            a.[i] <- char ((int a.[i]) + 1)
            if a.[i] > 'z' then
                a.[i] <- 'a'
                go (i - 1)
    go (a.Length - 1)
    String(a)

let hasStraight (p:string) =
    seq {0 .. p.Length - 3}
    |> Seq.exists (fun i -> int p.[i] + 1 = int p.[i+1] && int p.[i] + 2 = int p.[i+2])

let invalid = set ['i'; 'o'; 'l']
let containsInvalid (p:string) = p |> Seq.exists (fun c -> Set.contains c invalid)

let hasTwoPairs (p:string) =
    let rec loop i cnt =
        if i >= p.Length - 1 then cnt >= 2
        elif p.[i] = p.[i+1] then loop (i + 2) (cnt + 1)
        else loop (i + 1) cnt
    loop 0 0

let isValid p = hasStraight p && not (containsInvalid p) && hasTwoPairs p

let rec nextValid p =
    let p' = increment p
    if isValid p' then p' else nextValid p'

[<EntryPoint>]
let main _ =
    let pwd = File.ReadAllText("input.txt").Trim()
    let first = nextValid pwd
    let second = nextValid first
    printfn "%s" second
    0
