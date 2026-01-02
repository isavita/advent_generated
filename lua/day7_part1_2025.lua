
local function main()
    -- read whole input
    local lines = {}
    for line in io.lines("input.txt") do
        table.insert(lines, line)
    end
    local height = #lines
    if height == 0 then return end
    local width = #lines[1]

    -- locate start (S)
    local sx, sy = nil, nil
    for y = 1, height do
        local row = lines[y]
        local pos = row:find('S', 1, true)
        if pos then
            sx, sy = pos, y
            break
        end
    end
    if not sx then return end

    -- active columns for the current row
    local active = {}
    for i = 1, width do active[i] = false end
    active[sx] = true

    local splits = 0
    local next = {}

    for y = sy, height do
        -- clear next
        for i = 1, width do next[i] = false end

        local row = lines[y]
        for x = 1, width do
            if active[x] then
                local ch = row:sub(x, x)
                if ch == '^' then
                    splits = splits + 1
                    if x > 1 then next[x - 1] = true end
                    if x < width then next[x + 1] = true end
                else
                    next[x] = true
                end
            end
        end

        -- swap tables
        active, next = next, active

        -- stop if nothing is active
        local any = false
        for i = 1, width do
            if active[i] then any = true; break end
        end
        if not any then break end
    end

    print(splits)
end

main()
