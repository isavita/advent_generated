
local function tokenize(expression)
    expression = expression:gsub("%(", "( "):gsub("%)", " )")
    local tokens = {}
    for token in expression:gmatch("%S+") do
        table.insert(tokens, token)
    end
    return tokens
end

local function applyOp(op, a, b)
    if op == "+" then
        return a + b
    elseif op == "*" then
        return a * b
    else
        error("Unknown operator: " .. op)
    end
end

local function evaluateTokens(tokens)
    local ops = {}
    local vals = {}

    for _, token in ipairs(tokens) do
        if token == "(" then
            table.insert(ops, token)
        elseif token == "+" or token == "*" then
            while #ops > 0 and ops[#ops] ~= "(" do
                local b = table.remove(vals)
                local a = table.remove(vals)
                local op = table.remove(ops)
                table.insert(vals, applyOp(op, a, b))
            end
            table.insert(ops, token)
        elseif token == ")" then
            while ops[#ops] ~= "(" do
                local b = table.remove(vals)
                local a = table.remove(vals)
                local op = table.remove(ops)
                table.insert(vals, applyOp(op, a, b))
            end
            table.remove(ops) -- Remove the opening '('
        else
            table.insert(vals, tonumber(token))
        end
    end

    while #ops > 0 do
        local b = table.remove(vals)
        local a = table.remove(vals)
        local op = table.remove(ops)
        table.insert(vals, applyOp(op, a, b))
    end

    return vals[1]
end

local function evaluate(expression)
    local tokens = tokenize(expression)
    return evaluateTokens(tokens)
end

local function main()
    local file = io.open("input.txt", "r")
    if not file then
        print("Error opening file")
        return
    end

    local sum = 0
    for line in file:lines() do
        sum = sum + evaluate(line)
    end

    file:close()
    print(sum)
end

main()
