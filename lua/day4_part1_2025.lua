
local function main()
    local f = io.open("input.txt", "r")
    if not f then os.exit(1) end
    local grid = {}
    for line in f:lines() do
        if line ~= "" then
            grid[#grid + 1] = line:gsub("\r", "")
        end
    end
    f:close()

    local rows = #grid
    if rows == 0 then print(0) return end
    local cols = #grid[1]

    local dx = { -1, -1, -1, 0, 0, 1, 1, 1 }
    local dy = { -1, 0, 1, -1, 1, -1, 0, 1 }

    local acc = 0
    for y = 1, rows do
        local row = grid[y]
        for x = 1, cols do
            if row:sub(x, x) == '@' then
                local cnt = 0
                for d = 1, 8 do
                    local nx = x + dx[d]
                    local ny = y + dy[d]
                    if nx >= 1 and nx <= cols and ny >= 1 and ny <= rows then
                        if grid[ny]:sub(nx, nx) == '@' then cnt = cnt + 1 end
                    end
                end
                if cnt < 4 then acc = acc + 1 end
            end
        end
    end
    print(acc)
end

main()
