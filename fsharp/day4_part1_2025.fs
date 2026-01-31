
open System.IO

let dx = [|-1;-1;-1; 0; 0; 1; 1; 1|]
let dy = [|-1; 0; 1;-1; 1;-1; 0; 1|]

let grid = File.ReadAllLines "input.txt" |> Array.map (fun s -> s.ToCharArray())
let rows, cols = grid.Length, if grid.Length = 0 then 0 else grid.[0].Length

let mutable acc = 0
for y in 0..rows-1 do
    for x in 0..cols-1 do
        if grid.[y].[x] = '@' then
            let mutable cnt = 0
            for d in 0..7 do
                let nx, ny = x + dx.[d], y + dy.[d]
                if nx >= 0 && nx < cols && ny >= 0 && ny < rows && grid.[ny].[nx] = '@' then
                    cnt <- cnt + 1
            if cnt < 4 then acc <- acc + 1
printfn "%d" acc
