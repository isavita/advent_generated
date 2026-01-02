
local function main()
    local f = io.open("input.txt", "r")
    if not f then return end
    local grid = {}
    local R, C = 0, 0
    for line in f:lines() do
        line = line:gsub("\r?\n$", "")
        R = R + 1
        grid[R] = {line:byte(1, -1)}
        if #grid[R] > C then C = #grid[R] end
    end
    f:close()
    if R == 0 or C == 0 then
        print("Total rolls removed: 0")
        return
    end

    local dr = {-1,-1,-1,0,0,1,1,1}
    local dc = {-1,0,1,-1,1,-1,0,1}
    local removed, changed = 0, 0

    repeat
        changed = 0
        for r = 1, R do
            local row = grid[r]
            for c = 1, C do
                if row[c] == string.byte('@') then
                    local cnt = 0
                    for k = 1, 8 do
                        local nr, nc = r + dr[k], c + dc[k]
                        if nr >= 1 and nr <= R and nc >= 1 and nc <= C and grid[nr][nc] == string.byte('@') then
                            cnt = cnt + 1
                        end
                    end
                    if cnt < 4 then
                        row[c] = string.byte('*')
                        changed = changed + 1
                    end
                end
            end
        end
        for r = 1, R do
            local row = grid[r]
            for c = 1, C do
                if row[c] == string.byte('*') then
                    row[c] = string.byte('.')
                    removed = removed + 1
                end
            end
        end
    until changed == 0

    print("Total rolls removed: " .. removed)
end

main()
