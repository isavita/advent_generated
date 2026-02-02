
open System
open System.IO

let countChars (line: string) (count: int array) =
    line.ToCharArray() |> Array.iter (fun c -> count.[int c - int 'a'] <- count.[int c - int 'a'] + 1)

let main() =
    use reader = File.OpenText("input.txt")
    let count = Array.init 26 (fun _ -> 0)
    let mutable sum = 0

    let mutable line = reader.ReadLine()
    while line <> null do
        if line = "" then
            sum <- sum + (count |> Array.sumBy (fun x -> if x > 0 then 1 else 0))
            count |> Array.iteri (fun i _ -> count.[i] <- 0)
        else
            countChars line count
        line <- reader.ReadLine()

    sum <- sum + (count |> Array.sumBy (fun x -> if x > 0 then 1 else 0))
    printfn "%d" sum

main()
