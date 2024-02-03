
function readAll(path)
    local file = io.open(path, "r")
    local content = file:read("*a")
    file:close()
    return content
end

function SetOf(b)
    local m = {}
    for i = 1, #b do
        m[b:sub(i, i)] = true
    end
    local unique = {}
    for k, _ in pairs(m) do
        table.insert(unique, k)
    end
    return unique
end

function firstNUnique(s, n)
    for i = n, #s do
        local sub = s:sub(i - n + 1, i)
        local b = SetOf(sub)
        if #b == #sub then
            return i
        end
    end
    return -1
end

local content = readAll("input.txt")
print(firstNUnique(content, 14))
