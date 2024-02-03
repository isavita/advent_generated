
local rules = {}
local ruleRegex = "(%d+) (%w+ %w+) bags?[,.]"

for line in io.lines("input.txt") do
    local container, contents = line:match("(.+) bags contain (.+)")
    if contents ~= "no other bags." then
        for count, color in contents:gmatch(ruleRegex) do
            count = tonumber(count)
            if not rules[container] then
                rules[container] = {}
            end
            table.insert(rules[container], {Color = color, Count = count})
        end
    end
end

local function countBags(color)
    local count = 1
    if rules[color] then
        for _, rule in ipairs(rules[color]) do
            count = count + rule.Count * countBags(rule.Color)
        end
    end
    return count
end

local totalBags = countBags("shiny gold") - 1
print(totalBags)
