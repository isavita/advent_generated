module Day1

let input = System.IO.File.ReadAllLines("input.txt")

let parseInput (input: string[]) =
    let rec parseInputHelper (input: string list) (acc: int list list) (current: int list) =
        match input with
        | [] -> List.rev (current :: acc)
        | "" :: t -> parseInputHelper t (current :: acc) []
        | h :: t -> parseInputHelper t acc (int h :: current)
    parseInputHelper (List.ofArray input) [] []

let calculateTotalCalories (input: int list list) =
    let sumCalories (calories: int list) = List.sum calories
    let sortedCalories = List.sortBy sumCalories input |> List.rev
    List.take 3 sortedCalories |> List.sumBy sumCalories

let result = input |> parseInput |> calculateTotalCalories
printfn "%A" result