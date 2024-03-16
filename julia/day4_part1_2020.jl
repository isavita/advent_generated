using Printf

function is_valid(passport, required_fields)
    for field in required_fields
        if !occursin("$field:", passport)
            return false
        end
    end
    return true
end

function main()
    passports = String[]
    passport = ""
    open("input.txt", "r") do file
        for line in eachline(file)
            if isempty(line)
                push!(passports, passport)
                passport = ""
            else
                passport *= " $line"
            end
        end
        if !isempty(passport)
            push!(passports, passport)
        end
    end

    valid_passports = 0
    required_fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    for p in passports
        if is_valid(p, required_fields)
            valid_passports += 1
        end
    end

    @printf("%d\n", valid_passports)
end

main()