
local function main()
    local file = io.open("input.txt", "r")
    if not file then
        error("Cannot open input.txt")
    end
    local input_str = file:read("*a")
    file:close()
    input_str = input_str:match("^%s*(.-)%s*$")

    local ground = {{"+"}}
    local max_x, min_x, max_y, min_y = 0, 0, 0, 20
    local x_offset, y_offset = 500, 0

    for line in input_str:gmatch("[^\n]+") do
        local v1, val1, v2, val2, val3 = line:match("(%a)=(%d+), (%a)=(%d+)..(%d+)")
        val1 = tonumber(val1)
        val2 = tonumber(val2)
        val3 = tonumber(val3)

        if v1 == "x" then
            local x = val1 - x_offset
            local y1 = val2 - y_offset
            local y2 = val3 - y_offset

            while x >= max_x do
                max_x = max_x + 1
                for j = 1, #ground do
                    table.insert(ground[j], '.')
                end
            end
            while x <= min_x do
                min_x = min_x - 1
                for j = 1, #ground do
                    table.insert(ground[j], 1, '.')
                end
            end
            while y2 > max_y do
                max_y = max_y + 1
                local new_row = {}
                local width = #ground[1] or 0
                for _ = 1, width do table.insert(new_row, '.') end
                table.insert(ground, new_row)
            end
            if y1 < min_y then min_y = y1 end

            for i = y1, y2 do
                if i >= 0 then
                   local row_idx = i + 1
                   local col_idx = x - min_x + 1
                   if ground[row_idx] then ground[row_idx][col_idx] = '#' end
                end
            end
        else -- v1 == "y"
            local y = val1 - y_offset
            local x1 = val2 - x_offset
            local x2 = val3 - x_offset

             while y > max_y do
                max_y = max_y + 1
                local new_row = {}
                local width = #ground[1] or 0
                for _ = 1, width do table.insert(new_row, '.') end
                table.insert(ground, new_row)
            end
           while x2 >= max_x do
                max_x = max_x + 1
                for j = 1, #ground do
                    table.insert(ground[j], '.')
                end
            end
            while x1 <= min_x do
                min_x = min_x - 1
                for j = 1, #ground do
                    table.insert(ground[j], 1, '.')
                end
            end

            if y >= 0 then
                local row_idx = y + 1
                for i = x1, x2 do
                    local col_idx = i - min_x + 1
                    if ground[row_idx] then ground[row_idx][col_idx] = '#' end
                end
            end
            if y < min_y then min_y = y end
        end
    end

    local water_count = 0
    local flow_count = 0 -- Keep track for internal logic, not final output
    local round_limit = 200000
    local rounds = 0

    local start_col_idx = -min_x + 1
    local grid_height = #ground

    while rounds < round_limit do
        rounds = rounds + 1
        if not ground[2] or ground[2][start_col_idx] == '|' then break end

        local can_move = true
        local x = start_col_idx
        local y = 2 -- Start below '+' (which is at y=1)
        local try_left = 0 -- 0: initial, 1: tried left, 2: tried right
        local grid_width = #ground[1] or 0

        while can_move do
            if y + 1 > grid_height or (ground[y+1] and ground[y + 1][x] == '|') then
                 if y >= min_y + 1 then -- Check row index against original min_y
                     if ground[y][x] ~= '|' then flow_count = flow_count + 1 end
                 end
                 ground[y][x] = '|'
                 can_move = false

            elseif not ground[y+1] or ground[y + 1][x] == '.' then
                 if y + 1 <= grid_height then
                    y = y + 1
                    try_left = 0
                 else -- Fell off bottom
                     if y >= min_y + 1 then
                        if ground[y][x] ~= '|' then flow_count = flow_count + 1 end
                     end
                     ground[y][x] = '|'
                     can_move = false
                 end

            elseif ground[y + 1][x] == '#' or ground[y + 1][x] == '~' then
                local G_left = (x > 1 and ground[y][x - 1]) or '#'
                local G_right = (x < grid_width and ground[y][x + 1]) or '#'

                if (try_left == 1 and G_left == '|') or
                   (try_left == 2 and G_right == '|') or
                   (G_right == '|' and G_left ~= '.') or
                   (G_left == '|' and G_right ~= '.') then

                    local changed_to_flow = false
                    if ground[y][x] ~= '|' then
                        if y >= min_y + 1 then flow_count = flow_count + 1 end
                        if ground[y][x] == '~' then water_count = water_count - 1 end
                        changed_to_flow = true
                    end
                    ground[y][x] = '|'
                    can_move = false

                    if changed_to_flow then
                        local i = x + 1
                        while i <= grid_width and ground[y][i] == '~' do
                            ground[y][i] = '|'
                            water_count = water_count - 1
                            if y >= min_y + 1 then flow_count = flow_count + 1 end
                            i = i + 1
                        end
                        i = x - 1
                        while i >= 1 and ground[y][i] == '~' do
                            ground[y][i] = '|'
                            water_count = water_count - 1
                            if y >= min_y + 1 then flow_count = flow_count + 1 end
                            i = i - 1
                        end
                    end

                elseif (try_left == 0 and G_left == '.') or (try_left == 1 and G_left == '.') then
                    x = x - 1
                    try_left = 1
                elseif (try_left == 0 and G_right == '.') or (try_left == 2 and G_right == '.') then
                    x = x + 1
                    try_left = 2
                else
                    can_move = false
                    if ground[y][x] ~= '~' then
                        water_count = water_count + 1
                    end
                    ground[y][x] = '~'
                end
            else
                 -- Should not happen given the checks above, maybe indicates error or edge case
                 -- For safety, mark as flow and stop
                 if y >= min_y + 1 then
                     if ground[y][x] ~= '|' then flow_count = flow_count + 1 end
                 end
                 ground[y][x] = '|'
                 can_move = false
            end

            if y > grid_height * 2 then -- Safety break
                 can_move = false
            end
        end
    end

    local final_water_count = 0
    local final_grid_width = #ground[1] or 0
    for r = min_y + 1, max_y + 1 do
        if ground[r] then
            for c = 1, final_grid_width do
                if ground[r][c] == '~' then
                    final_water_count = final_water_count + 1
                end
            end
        end
    end

    print(final_water_count)

end

main()
