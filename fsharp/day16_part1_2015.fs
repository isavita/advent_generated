
open System
open System.IO
open System.Text.RegularExpressions

[<EntryPoint>]
let main _ =
    // expected MFCSAM values
    let expected =
        dict [
            ("children",   3)
            ("cats",       7)
            ("samoyeds",   2)
            ("pomeranians",3)
            ("akitas",     0)
            ("vizslas",    0)
            ("goldfish",   5)
            ("trees",      3)
            ("cars",       2)
            ("perfumes",   1)
        ]

    // read all lines from input.txt
    let lines = File.ReadAllLines("input.txt")

    // pattern: Sue <id>: <prop1>: <val1>, <prop2>: <val2>, ...
    let lineRegex = Regex(@"^Sue (\d+): (.+)$", RegexOptions.Compiled)

    let mutable answer = None

    for line in lines do
        if answer.IsNone then
            let m = lineRegex.Match(line)
            if m.Success then
                let sueId = m.Groups.[1].Value
                let props = m.Groups.[2].Value.Split([|", ";|], StringSplitOptions.RemoveEmptyEntries)

                let allMatch =
                    props
                    |> Array.forall (fun p ->
                        let parts = p.Split([|": "|], StringSplitOptions.RemoveEmptyEntries)
                        let name = parts.[0]
                        let value = Int32.Parse(parts.[1])
                        expected.[name] = value)

                if allMatch then
                    answer <- Some sueId

    match answer with
    | Some id -> printfn "%s" id
    | None    -> ()

    0
