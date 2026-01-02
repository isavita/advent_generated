
local f = assert(io.open("input.txt", "r"))
local pos, cnt = 50, 0
for token in f:read("*a"):gmatch("%S+") do
    local dir, amt = token:sub(1,1), tonumber(token:sub(2))
    pos = (pos + (dir == "R" and amt or -amt)) % 100
    cnt = cnt + (pos == 0 and 1 or 0)
end
f:close()
print(cnt)
