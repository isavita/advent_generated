
open System.IO

[<EntryPoint>]
let main _ =
    let map = function 'X'->1|'Y'->2|'Z'->3|_->0
    let win = function ('A','Y')|('B','Z')|('C','X')->6|_->0
    let draw = function ('A','X')|('B','Y')|('C','Z')->3|_->0
    File.ReadAllLines("input.txt")
    |> Array.sumBy (fun s -> map s[2] + win (s[0],s[2]) + draw (s[0],s[2]))
    |> printfn "%d"
    0
