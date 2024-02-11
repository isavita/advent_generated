
module Day4

open System.Security.Cryptography
open System.Text

let input = System.IO.File.ReadAllText("input.txt")

let rec findHash prefix num =
    let md5 = MD5.Create()
    let inputBytes = Encoding.ASCII.GetBytes (prefix + num.ToString())
    let hashBytes = md5.ComputeHash(inputBytes)
    let hashString = System.BitConverter.ToString(hashBytes).Replace("-", "").ToLower()
    if hashString.StartsWith("00000") then num
    else findHash prefix (num + 1)

let result = findHash input 1
printfn "%d" result
