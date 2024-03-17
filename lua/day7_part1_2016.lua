function supportsTLS(ip)
    local insideBrackets = ip:gmatch("%[[a-z]+%]")
    local bracketContents = {}

    for bracketContent in insideBrackets do
        if containsABBA(bracketContent) then
            return false
        end
    end

    ip = ip:gsub("%[[a-z]+%]", "-")
    return containsABBA(ip)
end

function containsABBA(s)
    for i = 1, #s-3 do
        if s:sub(i, i) ~= s:sub(i+1, i+1) and s:sub(i, i) == s:sub(i+3, i+3) and s:sub(i+1, i+1) == s:sub(i+2, i+2) then
            return true
        end
    end
    return false
end

local file = io.open("input.txt", "r")
local tlsCount = 0

for line in file:lines() do
    if supportsTLS(line) then
        tlsCount = tlsCount + 1
    end
end

print(tlsCount)

file:close()