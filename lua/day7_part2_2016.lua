function supportsSSL(ip)
    local insideBrackets = ip:gmatch("%[[a-z]+%]")
    local bracketContents = {}
    for bracketContent in insideBrackets do
        table.insert(bracketContents, bracketContent)
    end

    ip = ip:gsub("%[[a-z]+%]", "-")
    local abas = findABAs(ip)
    for _, aba in ipairs(abas) do
        local bab = aba:sub(2, 2) .. aba:sub(1, 1) .. aba:sub(2, 2)
        for _, bracketContent in ipairs(bracketContents) do
            if string.find(bracketContent, bab) then
                return true
            end
        end
    end

    return false
end

function findABAs(s)
    local abas = {}
    for i = 1, #s - 2 do
        if s:sub(i, i) ~= s:sub(i + 1, i + 1) and s:sub(i, i) == s:sub(i + 2, i + 2) then
            table.insert(abas, s:sub(i, i + 2))
        end
    end
    return abas
end

local file = io.open("input.txt", "r")
if not file then
    error("Error opening file")
end

local sslCount = 0

for line in file:lines() do
    if supportsSSL(line) then
        sslCount = sslCount + 1
    end
end

print(sslCount)

file:close()