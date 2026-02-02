
open System
open System.IO
open System.Text.RegularExpressions

let validateYear min max (value: string) =
    match Int32.TryParse value with
    | true, year -> year >= min && year <= max
    | _ -> false

let validateHgt (value: string) =
    if value.EndsWith "cm" then
        match Int32.TryParse(value.Replace("cm", "")) with
        | true, hgt -> hgt >= 150 && hgt <= 193
        | _ -> false
    elif value.EndsWith "in" then
        match Int32.TryParse(value.Replace("in", "")) with
        | true, hgt -> hgt >= 59 && hgt <= 76
        | _ -> false
    else false

let validateHcl (value: string) = Regex.IsMatch(value, @"^#[0-9a-f]{6}$")
let validateEcl (value: string) = Array.exists ((=) value) [|"amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"|]
let validatePid (value: string) = Regex.IsMatch(value, @"^[0-9]{9}$")

let isValidPassport (passport: string) =
    let fields =
        passport.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun field ->
            let parts = field.Split(':')
            if parts.Length = 2 then Some (parts.[0], parts.[1]) else None)
        |> Array.choose id
        |> Map.ofArray

    fields.ContainsKey "byr" && validateYear 1920 2002 fields["byr"] &&
    fields.ContainsKey "iyr" && validateYear 2010 2020 fields["iyr"] &&
    fields.ContainsKey "eyr" && validateYear 2020 2030 fields["eyr"] &&
    fields.ContainsKey "hgt" && validateHgt fields["hgt"] &&
    fields.ContainsKey "hcl" && validateHcl fields["hcl"] &&
    fields.ContainsKey "ecl" && validateEcl fields["ecl"] &&
    fields.ContainsKey "pid" && validatePid fields["pid"]

let main () =
    let lines = File.ReadAllLines "input.txt"
    let mutable validPassports = 0
    let mutable passport = ""

    for line in lines do
        if line = "" then
            if isValidPassport passport then validPassports <- validPassports + 1
            passport <- ""
        else
            passport <- passport + " " + line

    if passport <> "" && isValidPassport passport then validPassports <- validPassports + 1
    printfn "%d" validPassports

main ()
