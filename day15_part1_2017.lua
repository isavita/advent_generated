
local file = io.open("input.txt", "r")
if not file then
    print("Error reading file")
    return
end

local genAStart = tonumber(file:read())
local genBStart = tonumber(file:read())

local genAFactor = 16807
local genBFactor = 48271
local modulus = 2147483647

local genA = genAStart
local genB = genBStart
local matches = 0

for i = 1, 40000000 do
    genA = (genA * genAFactor) % modulus
    genB = (genB * genBFactor) % modulus

    if genA % 0x10000 == genB % 0x10000 then
        matches = matches + 1
    end
end

print(matches)
