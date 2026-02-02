
open System
open System.IO

let blockHeight = 7
let shapeWidth  = 5
let maxSum      = 5

let parseLock (block: string[]) =
    Array.init shapeWidth (fun c ->
        let mutable cnt = 0
        let mutable r = 1
        while r < blockHeight && block.[r].[c] = '#' do
            cnt <- cnt + 1
            r <- r + 1
        cnt)

let parseKey (block: string[]) =
    Array.init shapeWidth (fun c ->
        let mutable cnt = 0
        let mutable r = 5
        while r >= 0 && block.[r].[c] = '#' do
            cnt <- cnt + 1
            r <- r - 1
        cnt)

[<EntryPoint>]
let main _ =
    let fileName = "input.txt"
    if not (File.Exists fileName) then
        printfn "0"
        0
    else
        let rawLines = File.ReadAllLines fileName
        let lines    = rawLines |> Array.filter (not << String.IsNullOrWhiteSpace)
        let n        = lines.Length

        if n = 0 || n % blockHeight <> 0 then
            printfn "0"
            0
        else
            let lockHeights = System.Collections.Generic.List<int[]>()
            let keyHeights  = System.Collections.Generic.List<int[]>()

            let rec process idx =
                if idx >= n then ()
                else
                    let block = lines.[idx .. idx + blockHeight - 1]
                    let isLock =
                        block.[0]
                        |> Seq.forall (fun c -> c = '#')
                    if isLock then
                        lockHeights.Add(parseLock block)
                    else
                        keyHeights.Add(parseKey block)
                    process (idx + blockHeight)

            process 0

            let mutable total = 0L
            for l in lockHeights do
                for k in keyHeights do
                    if not (Seq.exists2 (fun a b -> a + b > maxSum) l k) then
                        total <- total + 1L

            printfn "%d" total
            0
