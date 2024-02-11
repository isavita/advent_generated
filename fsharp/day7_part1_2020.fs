
module Day7

let input = System.IO.File.ReadAllLines "input.txt"

let parseRule (rule:string) =
    let splitRule = rule.Split(" contain ")
    let bagColor = splitRule.[0].Replace(" bags", "")
    let contents = splitRule.[1].Split(", ")
    let bags = 
        contents 
        |> Array.map (fun x -> 
            let bag = x.Replace(" bags", "").Replace(" bag", "").Replace(".", "")
            if bag = "no other" then ("", 0)
            else 
                let spaceIndex = bag.IndexOf(" ")
                let count = int bag.[0..spaceIndex-1]
                let color = bag.[spaceIndex+1..]
                (color, count)
        )
    (bagColor, bags)

let rules = input |> Array.map parseRule

let rec containsShinyGold (color:string) =
    rules 
    |> Array.tryFind (fun (bagColor, bags) -> 
        bagColor = color && 
        (bags |> Array.exists (fun (innerColor, _) -> innerColor = "shiny gold" || containsShinyGold innerColor))
    )
    |> Option.isSome

let answer = rules |> Array.filter (fun (bagColor, _) -> containsShinyGold bagColor) |> Array.length

printfn "%d" answer
