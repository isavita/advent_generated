
open System
open System.IO

type Coord = { mutable x:int; mutable y:int; mutable z:int }

type Brick = {
    mutable mini: Coord
    mutable maxi: Coord
    basedOn: Collections.Generic.List<Brick>
    support: Collections.Generic.List<Brick>
}

let parse (lines:string[]) =
    lines
    |> Array.map (fun line ->
        let parts = line.Split([|'~'|])
        let a = parts.[0].Split([|','|])
        let b = parts.[1].Split([|','|])
        let mini = { x = int a.[0]; y = int a.[1]; z = int a.[2] }
        let maxi = { x = int b.[0]; y = int b.[1]; z = int b.[2] }
        { mini = mini; maxi = maxi
          basedOn = Collections.Generic.List()
          support = Collections.Generic.List() })

let settle (bricks:Brick[]) =
    let sorted = bricks |> Array.sortBy (fun b -> b.maxi.z)
    for i = 0 to sorted.Length-1 do
        let cur = sorted.[i]
        let mutable supportZ = 0
        let based = Collections.Generic.List<Brick>()
        for j = i-1 downto 0 do
            let b = sorted.[j]
            let ix = max cur.mini.x b.mini.x <= min cur.maxi.x b.maxi.x
            let iy = max cur.mini.y b.mini.y <= min cur.maxi.y b.maxi.y
            if ix && iy then
                if b.maxi.z = supportZ then
                    based.Add(b)
                elif b.maxi.z > supportZ then
                    supportZ <- b.maxi.z
                    based.Clear()
                    based.Add(b)
        cur.basedOn.AddRange(based)
        for b in based do b.support.Add(cur)
        let delta = cur.maxi.z - cur.mini.z
        cur.mini.z <- supportZ + 1
        cur.maxi.z <- cur.mini.z + delta

let solve (lines:string[]) =
    let bricks = parse lines
    settle bricks
    bricks
    |> Array.filter (fun b ->
        b.support |> Seq.forall (fun sup -> sup.basedOn.Count >= 2))
    |> Array.length

[<EntryPoint>]
let main _ =
    let input = File.ReadAllLines "input.txt"
    printfn "%d" (solve input)
    0
