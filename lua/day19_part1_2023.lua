-- Parse the input file
local workflows = {}
local parts = {}
local inWorkflow

for line in io.lines("input.txt") do
    if line == "" then
        inWorkflow = false
    elseif inWorkflow == nil or inWorkflow then
        -- Parse workflow
        local name, rules = line:match("(%w+)%{(.*)%}")
        local parsedRules = {}
        for rule in rules:gmatch("[^,]+") do
            local cond, dest = rule:match("(.+):(.+)")
            if cond then
                table.insert(parsedRules, {cond = cond, dest = dest})
            else
                table.insert(parsedRules, {dest = rule})
            end
        end
        workflows[name] = parsedRules
    else
        -- Parse part
        local x, m, a, s = line:match("{%s*x=(%d+),%s*m=(%d+),%s*a=(%d+),%s*s=(%d+)%s*}")
        table.insert(parts, {x = tonumber(x), m = tonumber(m), a = tonumber(a), s = tonumber(s)})
    end
end

-- Process parts through workflows
local function processPart(part, workflow)
    for _, rule in ipairs(workflows[workflow]) do
        if rule.cond then
            local var, op, num = rule.cond:match("(%a)(%W)(%d+)")
            num = tonumber(num)
            if (op == ">" and part[var] > num) or (op == "<" and part[var] < num) then
                if rule.dest == "A" then return true end
                if rule.dest == "R" then return false end
                return processPart(part, rule.dest)
            end
        else
            if rule.dest == "A" then return true end
            if rule.dest == "R" then return false end
            return processPart(part, rule.dest)
        end
    end
end

-- Sum up ratings for accepted parts
local sum = 0
for _, part in ipairs(parts) do
    if processPart(part, "in") then
        sum = sum + part.x + part.m + part.a + part.s
    end
end

print(sum)
