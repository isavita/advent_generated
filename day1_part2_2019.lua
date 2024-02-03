
function processLine(line)
    return tonumber(line)
end

function calcFuelMass(mass)
    local fuel = math.floor(mass / 3) - 2
    if fuel <= 0 then
        return 0
    end
    return fuel + calcFuelMass(fuel)
end

function getTotal(masses)
    local total = 0
    for i = 1, #masses do
        total = total + calcFuelMass(masses[i])
    end
    return total
end

local file = io.open("input.txt", "r")
local masses = {}
for line in file:lines() do
    local n = processLine(line)
    table.insert(masses, n)
end
file:close()

local total = getTotal(masses)
print(total)
