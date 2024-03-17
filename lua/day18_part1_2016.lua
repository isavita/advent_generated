-- Read the first row from the input file
local function read_first_row(filename)
    local file = io.open(filename, "r")
    if not file then
        error("Failed to open file: " .. filename)
    end
    local first_row = file:read("*l")
    file:close()
    return first_row
end

-- Check if a tile is a trap
local function is_trap(left, center, right, row)
    local function safe_if_out_of_bounds(index)
        if index < 1 or index > #row then
            return "."
        end
        return row:sub(index, index)
    end

    return (safe_if_out_of_bounds(left) == "^" and row:sub(center, center) == "^" and safe_if_out_of_bounds(right) == ".") or
           (row:sub(center, center) == "^" and safe_if_out_of_bounds(right) == "^" and safe_if_out_of_bounds(left) == ".") or
           (safe_if_out_of_bounds(left) == "^" and row:sub(center, center) == "." and safe_if_out_of_bounds(right) == ".") or
           (safe_if_out_of_bounds(right) == "^" and row:sub(center, center) == "." and safe_if_out_of_bounds(left) == ".")
end

-- Count the number of safe tiles
local function count_safe_tiles(first_row, total_rows)
    local current_row = first_row
    local safe_count = 0
    for i = 1, #current_row do
        if current_row:sub(i, i) == "." then
            safe_count = safe_count + 1
        end
    end

    for i = 2, total_rows do
        local next_row = ""
        for j = 1, #current_row do
            if is_trap(j - 1, j, j + 1, current_row) then
                next_row = next_row .. "^"
            else
                next_row = next_row .. "."
                safe_count = safe_count + 1
            end
        end
        current_row = next_row
    end

    return safe_count
end

-- Main function
local total_rows = 40
local first_row = read_first_row("input.txt")
local safe_tiles_count = count_safe_tiles(first_row, total_rows)
print(safe_tiles_count)