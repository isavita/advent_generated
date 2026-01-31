
open System
open System.IO

let totalRows = 40

let isTrap (l:char) (c:char) (r:char) =
    (l = '^' && c = '^' && r = '.') ||
    (c = '^' && r = '^' && l = '.') ||
    (l = '^' && c = '.' && r = '.') ||
    (r = '^' && c = '.' && l = '.')

let safeChar (row:char[]) (i:int) =
    if i < 0 || i >= row.Length then '.' else row.[i]

let countSafe (firstRow:string) =
    let mutable current = firstRow.ToCharArray()
    let mutable safe = Array.fold (fun acc ch -> if ch = '.' then acc+1 else acc) 0 current
    for _ = 1 to totalRows-1 do
        let next = Array.init current.Length (fun i ->
            if isTrap (safeChar current (i-1)) current.[i] (safeChar current (i+1)) then '^' else '.')
        safe <- safe + (Array.fold (fun acc ch -> if ch = '.' then acc+1 else acc) 0 next)
        current <- next
    safe

[<EntryPoint>]
let main _ =
    let line = File.ReadAllText("input.txt").TrimEnd('\n','\r')
    printfn "%d" (countSafe line)
    0
