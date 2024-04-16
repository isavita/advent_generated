function minMax(a, b)
    if a < b then
        return a, b
    else
        return b, a
    end
end

function giveChip(bots, outputs, target, value)
    if target:sub(1, 3) == "bot" then
        if not bots[target] then
            bots[target] = {chips = {}}
        end
        table.insert(bots[target].chips, value)
    elseif target:sub(1, 6) == "output" then
        if not outputs[target] then
            outputs[target] = 0
        end
        outputs[target] = value
    end
end

local bots = {}
local outputs = {}
local file = io.open("input.txt", "r")

for line in file:lines() do
    if line:match("value (%d+) goes to (bot %d+)") then
        local value, botID = line:match("value (%d+) goes to (bot %d+)")
        value = tonumber(value)
        giveChip(bots, outputs, botID, value)
    elseif line:match("(bot %d+) gives low to ([^ ]+ %d+) and high to ([^ ]+ %d+)") then
        local botID, lowTo, highTo = line:match("(bot %d+) gives low to ([^ ]+ %d+) and high to ([^ ]+ %d+)")
        if not bots[botID] then
            bots[botID] = {chips = {}}
        end
        bots[botID].lowTo = lowTo
        bots[botID].highTo = highTo
    end
end

file:close()

while true do
    local action = false
    for botID, b in pairs(bots) do
        if #b.chips == 2 then
            action = true
            local low, high = minMax(b.chips[1], b.chips[2])
            b.chips = {}
            giveChip(bots, outputs, b.lowTo, low)
            giveChip(bots, outputs, b.highTo, high)
        end
    end
    if not action then
        break
    end
end

if outputs["output 0"] and outputs["output 1"] and outputs["output 2"] then
    local result = outputs["output 0"] * outputs["output 1"] * outputs["output 2"]
    print(result)
else
    print("One or more outputs are missing.")
end