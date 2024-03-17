local input = {}
for line in io.lines("input.txt") do
    table.insert(input, line)
end

local monkeys = {}
for _, line in ipairs(input) do
    local name, job = line:match("(%w+): (.*)")
    monkeys[name] = job
end

local function solve(name)
    local job = monkeys[name]
    if tonumber(job) then
        return tonumber(job)
    else
        local a, op, b = job:match("(%w+) ([+%-*/]) (%w+)")
        local x = solve(a)
        local y = solve(b)
        if op == "+" then
            return x + y
        elseif op == "-" then
            return x - y
        elseif op == "*" then
            return x * y
        elseif op == "/" then
            return x / y
        end
    end
end

print(solve("root"))