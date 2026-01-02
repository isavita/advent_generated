
local function is_invalid(x)
    local s = tostring(x)
    local n = #s
    if n <= 1 then return false end
    for p = 1, n // 2 do
        if n % p ~= 0 then goto continue end
        local ok = true
        for i = p + 1, n do
            if s:sub(i, i) ~= s:sub(((i - 1) % p) + 1, ((i - 1) % p) + 1) then
                ok = false
                break
            end
        end
        if ok then return true end
        ::continue::
    end
    return false
end

local f = io.open("input.txt", "rb")
if not f then os.exit(1) end
local data = f:read("*a")
f:close()

local sum = 0

for aStr, bStr in data:gmatch("(%d+)%-(%d+)") do
    local a = tonumber(aStr)
    local b = tonumber(bStr)
    if a > b then a, b = b, a end
    local x = a
    while true do
        if is_invalid(x) then sum = sum + x end
        if x == b then break end
        if x == 0xFFFFFFFFFFFFFFFF then break end -- overflow safety
        x = x + 1
    end
end

print(sum)
