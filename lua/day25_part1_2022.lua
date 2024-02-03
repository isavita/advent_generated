
function fromSnafu(s)
    local n = 0
    for i = 1, #s do
        n = n * 5
        local c = string.sub(s, i, i)
        if c == '=' then
            n = n - 2
        elseif c == '-' then
            n = n - 1
        else
            n = n + tonumber(c)
        end
    end
    return n
end

function toSnafu(n)
    local b = {}
    while n > 0 do
        local remainder = n % 5
        if remainder == 3 then
            n = n + 5
            table.insert(b, '=')
        elseif remainder == 4 then
            n = n + 5
            table.insert(b, '-')
        else
            table.insert(b, tostring(remainder))
        end
        n = math.floor(n / 5)
    end
    for i = 1, math.floor(#b / 2) do
        b[i], b[#b - i + 1] = b[#b - i + 1], b[i]
    end
    return table.concat(b)
end

local sum = 0
local file = io.open("input.txt", "r")
if file then
    for line in file:lines() do
        sum = sum + fromSnafu(line)
    end
    file:close()
    print(toSnafu(sum))
else
    print("Error: Unable to open file")
end
