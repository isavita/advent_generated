local file = io.open("input.txt", "r")
local data = file:read("*a")
file:close()

local function decompress(str)
    local result = ""
    local i = 1
    while i <= #str do
        if str:sub(i, i) == "(" then
            local j = i + 1
            while str:sub(j, j) ~= ")" do
                j = j + 1
            end
            local marker = str:sub(i + 1, j - 1)
            local len, repeat_count = marker:match("(%d+)x(%d+)")
            len, repeat_count = tonumber(len), tonumber(repeat_count)
            result = result .. str:sub(j + 1, j + len):rep(repeat_count)
            i = j + len + 1
        else
            result = result .. str:sub(i, i)
            i = i + 1
        end
    end
    return result
end

print(#decompress(data))