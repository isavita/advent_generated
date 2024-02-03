
local file = io.open("input.txt", "r")
local totalElves = tonumber(file:read())
file:close()

local highestPowerOfTwo = 1
while highestPowerOfTwo * 2 <= totalElves do
    highestPowerOfTwo = highestPowerOfTwo * 2
end

local winner = (totalElves - highestPowerOfTwo) * 2 + 1
print(winner)
