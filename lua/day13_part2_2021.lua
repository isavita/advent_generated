local function read_input(filename)
    local dots = {}
    local folds = {}
    local is_folding_part = false
    for line in io.lines(filename) do
        if line == "" then
            is_folding_part = true
        elseif not is_folding_part then
            local x, y = line:match("(%d+),(%d+)")
            dots[#dots + 1] = {x = tonumber(x), y = tonumber(y)}
        else
            local axis, value = line:match("fold along (%w)=(%d+)")
            folds[#folds + 1] = {axis = axis, value = tonumber(value)}
        end
    end
    return dots, folds
end

local function apply_fold(dots, fold)
    local new_dots = {}
    local seen = {}
    for _, dot in ipairs(dots) do
        local new_x, new_y = dot.x, dot.y
        if fold.axis == "x" and dot.x > fold.value then
            new_x = 2 * fold.value - dot.x
        elseif fold.axis == "y" and dot.y > fold.value then
            new_y = 2 * fold.value - dot.y
        end
        local key = new_x .. "," .. new_y
        if not seen[key] then
            new_dots[#new_dots + 1] = {x = new_x, y = new_y}
            seen[key] = true
        end
    end
    return new_dots
end

local function print_dots(dots)
    local max_x, max_y = 0, 0
    for _, dot in ipairs(dots) do
        max_x = math.max(max_x, dot.x)
        max_y = math.max(max_y, dot.y)
    end
    local grid = {}
    for y = 0, max_y do
        grid[y] = {}
        for x = 0, max_x do
            grid[y][x] = "."
        end
    end
    for _, dot in ipairs(dots) do
        grid[dot.y][dot.x] = "#"
    end
    for y = 0, max_y do
        for x = 0, max_x do
            io.write(grid[y][x])
        end
        io.write("\n")
    end
end

local function main()
    local dots, folds = read_input("input.txt")
    local first_fold = true
    for _, fold in ipairs(folds) do
        dots = apply_fold(dots, fold)
        if first_fold then
            print("Dots visible after the first fold:", #dots)
            first_fold = false
        end
    end
    print("Final pattern:")
    print_dots(dots)
end

main()