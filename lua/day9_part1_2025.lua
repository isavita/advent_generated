
local function main()
    local f = assert(io.open("input.txt", "r"))
    local data = f:read("*a")
    f:close()

    local xs, ys = {}, {}
    for x, y in data:gmatch("(-?%d+),%s*(-?%d+)") do
        xs[#xs + 1] = tonumber(x)
        ys[#ys + 1] = tonumber(y)
    end

    local n = #xs
    local best = 0

    for i = 1, n do
        local xi, yi = xs[i], ys[i]
        for j = i, n do
            local dx = math.abs(xi - xs[j]) + 1
            local dy = math.abs(yi - ys[j]) + 1
            local area = dx * dy
            if area > best then best = area end
        end
    end

    print(best)
end

main()
