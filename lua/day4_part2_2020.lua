-- Function to check if a string represents a number in a given range
local function isValidNumber(str, min, max)
    local num = tonumber(str)
    return num and num >= min and num <= max
end

-- Function to validate height
local function isValidHeight(height)
    local num, unit = height:match("(%d+)(%a+)")
    if unit == "cm" then
        return isValidNumber(num, 150, 193)
    elseif unit == "in" then
        return isValidNumber(num, 59, 76)
    end
    return false
end

-- Function to validate hair color
local function isValidHairColor(color)
    return color:match("#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]") ~= nil
end

-- Function to validate eye color
local function isValidEyeColor(color)
    return color == "amb" or color == "blu" or color == "brn" or color == "gry" or 
           color == "grn" or color == "hzl" or color == "oth"
end

-- Function to validate passport ID
local function isValidPassportId(pid)
    return pid:match("^%d%d%d%d%d%d%d%d%d$") ~= nil
end

-- Main validation function
local function isValidPassport(passport)
    local requiredFields = {
        byr = function(v) return isValidNumber(v, 1920, 2002) end,
        iyr = function(v) return isValidNumber(v, 2010, 2020) end,
        eyr = function(v) return isValidNumber(v, 2020, 2030) end,
        hgt = isValidHeight,
        hcl = isValidHairColor,
        ecl = isValidEyeColor,
        pid = isValidPassportId
    }

    for key, validator in pairs(requiredFields) do
        if not passport[key] or not validator(passport[key]) then
            return false
        end
    end
    return true
end

-- Read file and process passports
local validCountPart1, validCountPart2 = 0, 0
local passport = {}
for line in io.lines("input.txt") do
    if line ~= "" then
        for pair in line:gmatch("([^%s]+)") do
            local key, value = pair:match("([^:]+):([^:]+)")
            passport[key] = value
        end
    else
        -- Check with data validation for part 2
        if isValidPassport(passport) then
            validCountPart2 = validCountPart2 + 1
        end
        
        passport = {}
    end
end

-- Check the last passport
if isValidPassport(passport) then
    validCountPart2 = validCountPart2 + 1
end

print(validCountPart2)
