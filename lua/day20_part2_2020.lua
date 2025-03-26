
local math = require("math")

local function split_multisep(str, sep)
     local result = {}
     local start = 1
     while true do
         local pos = str:find(sep, start, true) -- plain search
         if not pos then
             table.insert(result, str:sub(start))
             break
         end
         table.insert(result, str:sub(start, pos - 1))
         start = pos + #sep
     end
     -- Remove trailing empty string if input ends with separator
     if #result > 0 and result[#result] == "" then
         table.remove(result)
     end
     return result
end

local function split(str, sep)
    local result = {}
    local pattern = string.format("([^%s]+)", sep)
    for part in string.gmatch(str, pattern) do
        table.insert(result, part)
    end
    return result
end

local function parse_tiles_from_input(input_str)
    local tiles = {}
    for _, block in ipairs(split_multisep(input_str, "\n\n")) do
        if block ~= "" then
            local lines = split(block, "\n")
            if #lines > 1 then
                local id_str = lines[1]:match("Tile (%d+):")
                local tile_id = tonumber(id_str)
                local contents = {}
                for i = 2, #lines do
                    local row = {}
                    for j = 1, #lines[i] do
                        table.insert(row, lines[i]:sub(j, j))
                    end
                    table.insert(contents, row)
                end
                table.insert(tiles, {id = tile_id, contents = contents})
            end
        end
    end
    return tiles
end

local function get_col(grid, first_col)
    local col_chars = {}
    local col_index = first_col and 1 or #grid[1]
    for i = 1, #grid do
        table.insert(col_chars, grid[i][col_index])
    end
    return table.concat(col_chars)
end

local function get_row(grid, first_row)
    local row_index = first_row and 1 or #grid
    return table.concat(grid[row_index])
end

local function remove_borders_from_grid(grid)
    local new_grid = {}
    for r = 2, #grid - 1 do
        local new_row = {}
        for c = 2, #grid[r] - 1 do
            table.insert(new_row, grid[r][c])
        end
        table.insert(new_grid, new_row)
    end
    return new_grid
end

local function reverse_table(tbl)
    local reversed = {}
    for i = #tbl, 1, -1 do
        table.insert(reversed, tbl[i])
    end
    return reversed
end

local function mirror_string_grid(grid)
    local mirrored = {}
    for i = 1, #grid do
        table.insert(mirrored, reverse_table(grid[i]))
    end
    return mirrored
end

local function rotate_string_grid(grid)
    local rows = #grid
    local cols = #grid[1]
    local rotated = {}
    for c = 1, cols do
        local new_row = {}
        for r = rows, 1, -1 do
            table.insert(new_row, grid[r][c])
        end
        table.insert(rotated, new_row)
    end
    return rotated
end

local function all_grid_orientations(grid)
    local orientations = {}
    local current = grid
    for _ = 1, 4 do
        table.insert(orientations, current)
        current = rotate_string_grid(current)
    end
    local mirrored_orientations = {}
    for i = 1, 4 do
         table.insert(mirrored_orientations, mirror_string_grid(orientations[i]))
    end
    for _, mo in ipairs(mirrored_orientations) do
        table.insert(orientations, mo)
    end
    return orientations
end

local backtrack_assemble -- Forward declaration for recursion

backtrack_assemble = function(tiles, assembled_tiles, used_indices, edge_size)
    if not assembled_tiles then
        edge_size = math.floor(math.sqrt(#tiles) + 0.5)
        assembled_tiles = {}
        for r = 1, edge_size do
            assembled_tiles[r] = {}
            for c = 1, edge_size do
                assembled_tiles[r][c] = nil
            end
        end
        used_indices = {}
    end

    for r = 1, edge_size do
        for c = 1, edge_size do
            if assembled_tiles[r][c] == nil then
                for i = 1, #tiles do
                    if not used_indices[i] then
                        local t = tiles[i]
                        for _, opt in ipairs(all_grid_orientations(t.contents)) do
                            local fits = true
                            if r > 1 then
                                local current_top_row = get_row(opt, true)
                                local bottom_of_above = get_row(assembled_tiles[r-1][c].contents, false)
                                if current_top_row ~= bottom_of_above then
                                    fits = false
                                end
                            end
                            if fits and c > 1 then
                                local current_left_col = get_col(opt, true)
                                local right_col_of_left = get_col(assembled_tiles[r][c-1].contents, false)
                                if current_left_col ~= right_col_of_left then
                                    fits = false
                                end
                            end

                            if fits then
                                local placed_tile = {id = t.id, contents = opt}
                                assembled_tiles[r][c] = placed_tile
                                used_indices[i] = true
                                local result = backtrack_assemble(tiles, assembled_tiles, used_indices, edge_size)
                                if result then
                                    return result
                                end
                                assembled_tiles[r][c] = nil
                                used_indices[i] = nil
                            end
                        end
                    end
                end
                if assembled_tiles[r][c] == nil then
                    return nil
                end
            end
        end
    end
    return assembled_tiles
end

local function find_monster_coords(image)
    local monster_pattern = {
        "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   ",
    }
    local monster_offsets = {}
    local monster_height = #monster_pattern
    local monster_length = #monster_pattern[1]
    for r = 1, monster_height do
        for c = 1, monster_length do
            if monster_pattern[r]:sub(c, c) == "#" then
                table.insert(monster_offsets, {r = r, c = c})
            end
        end
    end

    local image_height = #image
    local image_width = #image[1]
    local monster_coords_set = {}

    for r = 1, image_height - monster_height + 1 do
        for c = 1, image_width - monster_length + 1 do
            local monster_found = true
            for _, offset in ipairs(monster_offsets) do
                 if image[r + offset.r - 1][c + offset.c - 1] ~= "#" then
                    monster_found = false
                    break
                end
            end
            if monster_found then
                for _, offset in ipairs(monster_offsets) do
                    local coord_r, coord_c = r + offset.r - 1, c + offset.c - 1
                    local key = coord_r .. "," .. coord_c
                    monster_coords_set[key] = {r = coord_r, c = coord_c}
                end
            end
        end
    end

     local monster_coords = {}
     for _, coord in pairs(monster_coords_set) do
         table.insert(monster_coords, coord)
     end

    return monster_coords
end

local function solve(input_str)
    local tiles = parse_tiles_from_input(input_str)
    local edge_size = math.floor(math.sqrt(#tiles) + 0.5)

    local assembled_tiles = backtrack_assemble(tiles)

    for r = 1, edge_size do
        for c = 1, edge_size do
            assembled_tiles[r][c].contents = remove_borders_from_grid(assembled_tiles[r][c].contents)
        end
    end

    local image = {}
    local sub_grid_size = #assembled_tiles[1][1].contents
    if sub_grid_size > 0 then
        for big_row = 1, edge_size do
            for sub_row = 1, sub_grid_size do
                local image_row = {}
                for big_col = 1, edge_size do
                    local sub_line = assembled_tiles[big_row][big_col].contents[sub_row]
                    for i = 1, #sub_line do
                        table.insert(image_row, sub_line[i])
                    end
                end
                table.insert(image, image_row)
            end
        end
    end

    local final_image = nil
    local monster_coords = {}
    for _, opt in ipairs(all_grid_orientations(image)) do
        local current_monster_coords = find_monster_coords(opt)
        if #current_monster_coords > 0 then
            final_image = opt
            monster_coords = current_monster_coords
            break
        end
    end

    for _, coord in ipairs(monster_coords) do
        final_image[coord.r][coord.c] = "O"
    end

    local rough_waters_count = 0
    for r = 1, #final_image do
        for c = 1, #final_image[r] do
            if final_image[r][c] == "#" then
                rough_waters_count = rough_waters_count + 1
            end
        end
    end

    return rough_waters_count
end

local function main()
    local file = io.open("input.txt", "r")
    if not file then
        error("Could not open input.txt")
        return
    end
    local input_str = file:read("*a")
    file:close()
    input_str = input_str:match("^%s*(.-)%s*$")

    local result = solve(input_str)
    print(result)
end

main()
