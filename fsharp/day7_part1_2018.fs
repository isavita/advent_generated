
open System
open System.IO
open System.Text

[<EntryPoint>]
let main _ =
    let maxSteps = 26
    let graph = Array.init maxSteps (fun _ -> Array.zeroCreate<int> maxSteps)
    let indeg = Array.zeroCreate<int> maxSteps
    for line in File.ReadAllLines "input.txt" do
        let s1 = line.[5]
        let s2 = line.[36]
        let i = int s1 - int 'A'
        let j = int s2 - int 'A'
        graph.[i].[j] <- 1
        indeg.[j] <- indeg.[j] + 1
    let sb = StringBuilder()
    let mutable completed = 0
    while completed < maxSteps do
        let i = indeg |> Array.findIndex ((=) 0)
        sb.Append(char (i + int 'A')) |> ignore
        indeg.[i] <- -1
        completed <- completed + 1
        for j = 0 to maxSteps - 1 do
            if graph.[i].[j] = 1 then indeg.[j] <- indeg.[j] - 1
    Console.WriteLine(sb.ToString())
    0
