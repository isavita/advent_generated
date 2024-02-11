
module Day4

open System.Security.Cryptography
open System.Text
open System

let input = System.IO.File.ReadAllText("input.txt")

let rec findAdventCoin prefix num =
    let md5 = MD5.Create()
    let inputBytes = Encoding.ASCII.GetBytes (prefix + num.ToString())
    let hashBytes = md5.ComputeHash(inputBytes)
    let hashString = BitConverter.ToString(hashBytes).Replace("-", "").ToLower()
    if hashString.StartsWith("00000") then num
    else findAdventCoin prefix (num + 1)

let part1 = findAdventCoin input 1

let rec findAdventCoinSixZeroes prefix num =
    let md5 = MD5.Create()
    let inputBytes = Encoding.ASCII.GetBytes (prefix + num.ToString())
    let hashBytes = md5.ComputeHash(inputBytes)
    let hashString = BitConverter.ToString(hashBytes).Replace("-", "").ToLower()
    if hashString.StartsWith("000000") then num
    else findAdventCoinSixZeroes prefix (num + 1)

let part2 = findAdventCoinSixZeroes input 1

printfn "%d" part1
printfn "%d" part2
