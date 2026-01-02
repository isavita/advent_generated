
local function main()
    local f = io.open("input.txt", "r")
    if not f then io.stderr:write("cannot open input.txt\n") return 1 end

    local grid = {}
    for line in f:lines() do
        if #line > 0 then
            grid[#grid + 1] = line
        end
    end
    f:close()

    local height = #grid
    if height == 0 then
        print(0)
        return
    end
    local width = #grid[1]

    local startX, startY = -1, -1
    for y = 1, height do
        local sx = grid[y]:find("S", 1, true)
        if sx then
            startX = sx - 1          -- zero‑based
            startY = y - 1
            break
        end
    end
    if startX < 0 then
        io.stderr:write("Start point 'S' not found\n")
        return 1
    end

    local counts = {[startX] = 1}

    for y = startY, height - 1 do
        local row = grid[y + 1]
        local next = {}
        for x, cnt in pairs(counts) do
            local splitter = (x >= 0 and x < width and row:sub(x + 1, x + 1) == "^")
            if splitter then
                local left, right = x - 1, x + 1
                next[left] = (next[left] or 0) + cnt
                next[right] = (next[right] or 0) + cnt
            else
                next[x] = (next[x] or 0) + cnt
            end
        end
        counts = next
    end

    local total = 0
    for _, cnt in pairs(counts) do total = total + cnt end
    print(total)
end

main()
