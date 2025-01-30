
function tokenize(s)
    local tokens = {}
    local i = 1
    while i <= #s do
        local c = s:sub(i, i)
        if c:match("%d") then
            local num = ""
            while i <= #s and s:sub(i, i):match("%d") do
                num = num .. s:sub(i, i)
                i = i + 1
            end
            table.insert(tokens, num)
        else
            if c == '+' or c == '*' or c == '(' or c == ')' then
                table.insert(tokens, c)
                i = i + 1
            else
                i = i + 1
            end
        end
    end
    return tokens
end

function infix_to_postfix(tokens)
    local output = {}
    local stack = {}
    local precedence = { ['+'] = 2, ['*'] = 1 }
    for i, token in ipairs(tokens) do
        if tonumber(token) ~= nil then
            table.insert(output, token)
        elseif token == '(' then
            table.insert(stack, token)
        elseif token == ')' then
            while stack[#stack] ~= '(' do
                table.insert(output, table.remove(stack))
                if #stack == 0 then
                    error("Mismatched parentheses")
                end
            end
            table.remove(stack)  -- remove '('
        else
            -- operator
            while #stack > 0 and stack[#stack] ~= '(' and precedence[token] <= precedence[stack[#stack]] do
                table.insert(output, table.remove(stack))
            end
            table.insert(stack, token)
        end
    end
    -- Pop remaining operators
    while #stack > 0 do
        table.insert(output, table.remove(stack))
    end
    return output
end

function evaluate_postfix(postfix)
    local stack = {}
    for i, token in ipairs(postfix) do
        if tonumber(token) ~= nil then
            table.insert(stack, tonumber(token))
        else
            local b = table.remove(stack)
            local a = table.remove(stack)
            if token == '+' then
                table.insert(stack, a + b)
            elseif token == '*' then
                table.insert(stack, a * b)
            else
                error("Unknown operator: " .. token)
            end
        end
    end
    return table.remove(stack)
end

local sum = 0
local file = io.open("input.txt", "r")
for line in file:lines() do
    line = line:gsub("%s+", "")  -- remove any whitespace
    if line == "" then
        goto continue
    end
    local tokens = tokenize(line)
    local postfix = infix_to_postfix(tokens)
    local result = evaluate_postfix(postfix)
    sum = sum + result
    ::continue::
end
file:close()
print(sum)
