
local file = io.open("input.txt", "r")
local genAStart = tonumber(file:read())
local genBStart = tonumber(file:read())

local genAFactor = 16807
local genBFactor = 48271
local modulus = 2147483647

local genA = genAStart
local genB = genBStart
local matches = 0

for i = 1, 5000000 do
    repeat
        genA = (genA * genAFactor) % modulus
    until genA % 4 == 0

    repeat
        genB = (genB * genBFactor) % modulus
    until genB % 8 == 0

    if genA & 0xFFFF == genB & 0xFFFF then
        matches = matches + 1
    end
end

print(matches)
