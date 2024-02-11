
module Day5

open System.Security.Cryptography
open System.Text

let input = System.IO.File.ReadAllText("input.txt").Trim()

let md5(input: string) =
    use md5 = MD5.Create()
    md5.ComputeHash(Encoding.UTF8.GetBytes(input))
    |> Array.map (fun b -> b.ToString("x2"))
    |> String.concat ""

let rec findPassword index password =
    if String.length password = 8 then password
    else
        let hash = md5 (input + index.ToString())
        if hash.StartsWith("00000") then
            findPassword (index + 1) (password + hash.[5].ToString())
        else
            findPassword (index + 1) password

let result = findPassword 0 ""
printfn "%s" result
