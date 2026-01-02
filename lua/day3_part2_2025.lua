
local TARGET = 12

local function add_big(a, b)
    local i, j = #a, #b
    local carry = 0
    local res = {}
    while i > 0 or j > 0 or carry > 0 do
        local d1 = i > 0 and (a:byte(i) - 48) or 0
        local d2 = j > 0 and (b:byte(j) - 48) or 0
        local s = d1 + d2 + carry
        carry = s // 10
        table.insert(res, 1, string.char((s % 10) + 48))
        i = i - 1
        j = j - 1
    end
    return table.concat(res)
end

local total = "0"
local f = io.open("input.txt", "r")
if not f then os.exit(1) end

for line in f:lines() do
    line = line:gsub("%D+$", "")               -- strip trailing non‑digits
    if #line < TARGET then goto continue end

    local len = #line
    local rem = len - TARGET
    local stack = {}

    for i = 1, len do
        local ch = line:sub(i, i)
        while rem > 0 and #stack > 0 and stack[#stack] < ch do
            stack[#stack] = nil
            rem = rem - 1
        end
        stack[#stack + 1] = ch
    end

    while #stack > TARGET do               -- ensure exact length TARGET
        stack[#stack] = nil
    end

    local num = table.concat(stack)
    total = add_big(total, num)

    ::continue::
end

f:close()
print(total)
