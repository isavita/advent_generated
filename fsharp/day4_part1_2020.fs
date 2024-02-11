
module Day4

let input = System.IO.File.ReadAllText("input.txt")

let passports = input.Split("\n\n")

let isValidPassport (passport: string) =
    let fields = passport.Split([|' '; '\n'|])
    let requiredFields = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]
    requiredFields |> List.forall (fun field -> fields |> Array.exists (fun f -> f.StartsWith(field)))

let validPassports = passports |> Array.filter isValidPassport |> Array.length

printfn "%d" validPassports
