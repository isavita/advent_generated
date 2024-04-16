using DelimitedFiles

function isValidPassport(passport)
    fields = split(passport)
    fieldMap = Dict(split(field, ":")[1] => split(field, ":")[2] for field in fields)

    return validateByr(get(fieldMap, "byr", "")) &&
           validateIyr(get(fieldMap, "iyr", "")) &&
           validateEyr(get(fieldMap, "eyr", "")) &&
           validateHgt(get(fieldMap, "hgt", "")) &&
           validateHcl(get(fieldMap, "hcl", "")) &&
           validateEcl(get(fieldMap, "ecl", "")) &&
           validatePid(get(fieldMap, "pid", ""))
end

function validateByr(value)
    return validateYear(value, 1920, 2002)
end

function validateIyr(value)
    return validateYear(value, 2010, 2020)
end

function validateEyr(value)
    return validateYear(value, 2020, 2030)
end

function validateYear(value, min, max)
    year = tryparse(Int, value)
    return year !== nothing && min <= year <= max
end

function validateHgt(value)
    if endswith(value, "cm")
        hgt = tryparse(Int, value[1:end-2])
        return hgt !== nothing && 150 <= hgt <= 193
    elseif endswith(value, "in")
        hgt = tryparse(Int, value[1:end-2])
        return hgt !== nothing && 59 <= hgt <= 76
    end
    return false
end

function validateHcl(value)
    return occursin(r"^#[0-9a-f]{6}$", value)
end

function validateEcl(value)
    validEcl = Set(["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
    return value in validEcl
end

function validatePid(value)
    return occursin(r"^[0-9]{9}$", value)
end

passports = split(read("input.txt", String), "\n\n")
passports = [replace(passport, "\n" => " ") for passport in passports]

validPassports = count(isValidPassport, passports)
println(validPassports)