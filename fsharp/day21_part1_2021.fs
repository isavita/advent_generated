
open System.IO

[<EntryPoint>]
let main _ =
    let [| l1; l2 |] = File.ReadAllLines "input.txt"
    let p1Start = int (l1.Substring 28)
    let p2Start = int (l2.Substring 28)

    let rec loop p1 p2 s1 s2 roll rolls =
        if s1 >= 1000 then printfn "%d" (s2 * rolls)
        elif s2 >= 1000 then printfn "%d" (s1 * rolls)
        else
            let move = roll % 100 + (roll + 1) % 100 + (roll + 2) % 100
            let roll = roll + 3
            let rolls = rolls + 3
            if s1 < 1000 then
                let p1 = (p1 + move - 1) % 10 + 1
                let s1 = s1 + p1
                if s1 >= 1000 then printfn "%d" (s2 * rolls)
                else
                    let move = roll % 100 + (roll + 1) % 100 + (roll + 2) % 100
                    let roll = roll + 3
                    let rolls = rolls + 3
                    let p2 = (p2 + move - 1) % 10 + 1
                    let s2 = s2 + p2
                    loop p1 p2 s1 s2 roll rolls
            else loop p1 p2 s1 s2 roll rolls
    loop p1Start p2Start 0 0 1 0
    0
