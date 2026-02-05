
open System
open System.IO
open System.Collections.Generic

type File = { size: int }

type Directory() =
    member val Files = Dictionary<string, File>() with get, set
    member val Dirs  = Dictionary<string, Directory>() with get, set
    member this.TotalSize(sum: byref<int>) =
        let mutable size = 0
        for f in this.Files.Values do size <- size + f.size
        for d in this.Dirs.Values do size <- size + d.TotalSize(&sum)
        if size <= 100000 then sum <- sum + size
        size

[<EntryPoint>]
let main _ =
    let root = Directory()
    let mutable cur = root
    let stack = Stack<Directory>()
    stack.Push(root)

    for line in File.ReadLines("input.txt") do
        if line.StartsWith("$ cd") then
            let path = line.Substring(4).Trim()
            if path = "/" then
                cur <- root
                stack.Clear()
                stack.Push(root)
            elif path = ".." then
                stack.Pop() |> ignore
                cur <- stack.Peek()
            else
                if not (cur.Dirs.ContainsKey(path)) then
                    cur.Dirs.[path] <- Directory()
                cur <- cur.Dirs.[path]
                stack.Push(cur)
        elif line.StartsWith("dir ") then
            let name = line.Substring(4).Trim()
            cur.Dirs.[name] <- Directory()
        else
            let parts = line.Split([|' '|], 2)
            if parts.Length = 2 then
                match Int32.TryParse(parts.[0]) with
                | true, sz ->
                    let fname = parts.[1]
                    cur.Files.[fname] <- { size = sz }
                | _ -> ()

    let mutable sum = 0
    let _ = root.TotalSize(&sum)
    printfn "%d" sum
    0
