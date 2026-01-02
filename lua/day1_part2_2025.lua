
local function floorDiv(a, b) return a // b end

local function main()
    local f = io.open("input.txt", "r")
    if not f then error("cannot open input.txt") end
    local pos, hits = 50, 0
    for line in f:lines() do
        local dir, amt = line:match("([RL])%s*(-?%d+)")
        if dir then
            amt = tonumber(amt)
            if dir == 'R' then
                local new = pos + amt
                hits = hits + new // 100
                pos = new % 100
            else
                hits = hits + floorDiv(pos - 1, 100) - floorDiv(pos - amt - 1, 100)
                pos = (pos - amt) % 100
                if pos < 0 then pos = pos + 100 end
            end
        end
    end
    f:close()
    print(hits)
end

main()
