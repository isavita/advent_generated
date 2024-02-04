
import re

def is_valid_passport(passport):
    required_fields = {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}
    fields = re.findall(r"([a-z]{3}):", passport)
    return required_fields.issubset(fields)

with open("input.txt", "r") as file:
    data = file.read()

passports = data.split("\n\n")
valid_passports = sum(is_valid_passport(passport) for passport in passports)

print(valid_passports)
