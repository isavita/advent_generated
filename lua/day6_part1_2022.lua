
function readAll(path)
    local file = io.open(path, "r")
    local content = file:read("*all")
    file:close()
    return content
end

function firstNUnique(s, n)
    for i = n, #s do
        local b = string.sub(s, i - n + 1, i)
        local set = {}
        local isUnique = true
        for j = 1, #b do
            if not set[string.sub(b, j, j)] then
                set[string.sub(b, j, j)] = true
            else
                isUnique = false
                break
            end
        end
        if isUnique then
            return i
        end
    end
    return -1
end

local s = readAll("input.txt")
print(firstNUnique(s, 4))
