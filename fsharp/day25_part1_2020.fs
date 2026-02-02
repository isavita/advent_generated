
open System
open System.IO

let transform (subjectNumber: int64) (loopSize: int64) : int64 = 
    let rec loop (value: int64) (size: int64) : int64 =
        if size = 0L then value
        else loop ((value * subjectNumber) % 20201227L) (size - 1L)
    loop 1L loopSize

let findLoopSize (publicKey: int64) : int64 =
    let rec loop (value: int64) (size: int64) : int64 =
        if value = publicKey then size
        else loop ((value * 7L) % 20201227L) (size + 1L)
    loop 1L 0L

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines("input.txt")
    let cardPublicKey = int64 lines.[0]
    let doorPublicKey = int64 lines.[1]
    let loopSize = findLoopSize cardPublicKey
    let encryptionKey = transform doorPublicKey loopSize
    printfn "%d" encryptionKey
    0
