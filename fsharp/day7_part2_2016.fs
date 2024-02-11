module Day7

open System
open System.Text.RegularExpressions
open System.IO

let supportsSSL (ip:string) =
    let insideBrackets = new Regex @"\[[a-z]+\]"
    let bracketContents = insideBrackets.Matches(ip) |> Seq.map (fun m -> m.Value) |> Seq.toList

    let mutable abas = []
    let mutable ssl = false
    let mutable ip = insideBrackets.Replace(ip, "-")

    for i = 0 to ip.Length - 3 do
        if ip.[i] <> ip.[i+1] && ip.[i] = ip.[i+2] then
            abas <- abas @ [sprintf "%c%c%c" ip.[i] ip.[i+1] ip.[i+2]]

    for aba in abas do
        let bab = sprintf "%c%c%c" aba.[1] aba.[0] aba.[1]
        for bracketContent in bracketContents do
            if bracketContent.Contains(bab) then
                ssl <- true

    ssl

let mutable sslCount = 0
let file = File.OpenText("input.txt")
while not file.EndOfStream do
    let line = file.ReadLine()
    if supportsSSL(line) then
        sslCount <- sslCount + 1

Console.WriteLine(sslCount)