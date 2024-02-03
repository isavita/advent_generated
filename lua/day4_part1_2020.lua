
local file = assert(io.open("input.txt", "r"))
local passports = {}
local passport = ""

for line in file:lines() do
    if line == "" then
        table.insert(passports, passport)
        passport = ""
    else
        passport = passport .. " " .. line
    end
end
if passport ~= "" then
    table.insert(passports, passport)
end
file:close()

local function isValid(passport, requiredFields)
    for i, field in ipairs(requiredFields) do
        if not string.find(passport, field .. ":") then
            return false
        end
    end
    return true
end

local validPassports = 0
local requiredFields = {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}

for i, p in ipairs(passports) do
    if isValid(p, requiredFields) then
        validPassports = validPassports + 1
    end
end

print(validPassports)
