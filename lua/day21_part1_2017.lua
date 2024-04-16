local function split(inputstr, sep)
    if sep == nil then
        sep = "%s"
    end
    local t = {}
    for str in string.gmatch(inputstr, "([^" .. sep .. "]+)") do
        table.insert(t, str)
    end
    return t
end

local function read_rules(filename)
    local rules = {}
    for line in io.lines(filename) do
        local parts = split(line, " => ")
        rules[parts[1]] = parts[2]
    end
    return rules
end

local function rotate(pattern)
    local grid = split(pattern, '/')
    local n = #grid
    local new_grid = {}
    for i = 1, n do
        new_grid[i] = {}
        for j = 1, n do
            new_grid[i][j] = grid[n-j+1]:sub(i,i)
        end
        new_grid[i] = table.concat(new_grid[i])
    end
    return table.concat(new_grid, '/')
end

local function flip(pattern)
    local grid = split(pattern, '/')
    local n = #grid
    for i = 1, n do
        grid[i] = grid[i]:reverse()
    end
    return table.concat(grid, '/')
end

local function get_variations(pattern)
    local variations = {}
    local r = pattern
    for i = 1, 4 do
        r = rotate(r)
        variations[r] = true
        variations[flip(r)] = true
    end
    return variations
end

local function find_rule(pattern, rules)
    local variations = get_variations(pattern)
    for var, _ in pairs(variations) do
        if rules[var] then
            return rules[var]
        end
    end
    return nil
end

local function divide_and_enhance(grid, rules)
    local size = #grid
    local new_grid = {}
    local block_size = size % 2 == 0 and 2 or 3
    local new_block_size = block_size + 1
    local num_blocks = size / block_size

    for i = 1, num_blocks do
        for j = 1, num_blocks do
            local pattern = {}
            for k = 0, block_size - 1 do
                table.insert(pattern, grid[(i-1)*block_size + k + 1]:sub((j-1)*block_size + 1, j*block_size))
            end
            pattern = table.concat(pattern, '/')
            local new_pattern = find_rule(pattern, rules)
            local rows = split(new_pattern, '/')
            for k = 1, new_block_size do
                local idx = (i-1)*new_block_size + k
                new_grid[idx] = (new_grid[idx] or '') .. rows[k]
            end
        end
    end
    return new_grid
end

local function count_on_pixels(grid)
    local count = 0
    for i = 1, #grid do
        for pixel in grid[i]:gmatch("#") do
            count = count + 1
        end
    end
    return count
end

-- Main execution
local initial_pattern = ".#./..#/###"
local rules = read_rules("input.txt")
local grid = split(initial_pattern, '/')

for i = 1, 5 do
    grid = divide_and_enhance(grid, rules)
end

local result = count_on_pixels(grid)
print("Number of pixels that are on:", result)