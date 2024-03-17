-- Read input from "input.txt"
local serial_number = tonumber(io.open("input.txt", "r"):read("*l"))

-- Function to calculate power level of a fuel cell
local function power_level(x, y)
    local rack_id = x + 10
    local power = rack_id * y
    power = power + serial_number
    power = power * rack_id
    power = math.floor(power / 100) % 10
    power = power - 5
    return power
end

-- Find the 3x3 square with the largest total power
local max_power = -math.huge
local max_x, max_y = 0, 0
for x = 1, 298 do
    for y = 1, 298 do
        local total_power = 0
        for i = 0, 2 do
            for j = 0, 2 do
                total_power = total_power + power_level(x + i, y + j)
            end
        end
        if total_power > max_power then
            max_power = total_power
            max_x, max_y = x, y
        end
    end
end

-- Print the result
print(string.format("%d,%d", max_x, max_y))