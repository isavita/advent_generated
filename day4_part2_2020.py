with open("input.txt") as f:
    data = f.read()

passports = data.split("\n\n")

valid_passports = 0

for passport in passports:
    fields = passport.replace("\n", " ").split(" ")
    field_dict = {}
    for field in fields:
        key, value = field.split(":")
        field_dict[key] = value

    if all(field in field_dict for field in ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]):
        if (
            1920 <= int(field_dict["byr"]) <= 2002
            and 2010 <= int(field_dict["iyr"]) <= 2020
            and 2020 <= int(field_dict["eyr"]) <= 2030
            and (
                field_dict["hgt"][-2:] == "cm"
                and 150 <= int(field_dict["hgt"][:-2]) <= 193
                or field_dict["hgt"][-2:] == "in"
                and 59 <= int(field_dict["hgt"][:-2]) <= 76
            )
            and field_dict["hcl"][0] == "#"
            and len(field_dict["hcl"]) == 7
            and all(c in "0123456789abcdef" for c in field_dict["hcl"][1:])
            and field_dict["ecl"] in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
            and len(field_dict["pid"]) == 9
            and field_dict["pid"].isdigit()
        ):
            valid_passports += 1

print(valid_passports)