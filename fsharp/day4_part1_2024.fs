
open System.IO

let dirs = [|
    (0,1); (1,0); (1,1); (-1,1);
    (0,-1); (-1,0); (-1,-1); (1,-1)
|]

let check (grid:string[]) (w:string) x y (dx,dy) =
    let h,wid = grid.Length,grid.[0].Length
    [0..w.Length-1]
    |> Seq.forall (fun i ->
        let nx,ny = x+dx*i,y+dy*i
        nx>=0 && ny>=0 && nx<h && ny<wid && grid.[nx].[ny]=w.[i])

let countOcc (grid:string[]) word =
    seq{for x in 0..grid.Length-1 do
        for y in 0..grid.[0].Length-1 do
        for d in dirs do
        if check grid word x y d then yield ()}
    |> Seq.length

[<EntryPoint>]
let main _ =
    let grid = File.ReadAllLines "input.txt"
    printfn "%d" (countOcc grid "XMAS")
    0
