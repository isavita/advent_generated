-- Define the range for the reactor initialization procedure
local MIN, MAX = -50, 50

-- Function to read and parse the input file
local function read_input(filename)
    local steps = {}
    for line in io.lines(filename) do
        local on_off, x1, x2, y1, y2, z1, z2 = line:match("(%a+) x=(%-?%d+)..(%-?%d+),y=(%-?%d+)..(%-?%d+),z=(%-?%d+)..(%-?%d+)")
        table.insert(steps, {
            state = on_off == "on",
            x1 = tonumber(x1), x2 = tonumber(x2),
            y1 = tonumber(y1), y2 = tonumber(y2),
            z1 = tonumber(z1), z2 = tonumber(z2)
        })
    end
    return steps
end

-- Initialize the reactor grid
local function create_grid()
    local grid = {}
    for x = MIN, MAX do
        grid[x] = {}
        for y = MIN, MAX do
            grid[x][y] = {}
            for z = MIN, MAX do
                grid[x][y][z] = false
            end
        end
    end
    return grid
end

-- Apply a single step to the grid
local function apply_step(grid, step)
    for x = math.max(step.x1, MIN), math.min(step.x2, MAX) do
        for y = math.max(step.y1, MIN), math.min(step.y2, MAX) do
            for z = math.max(step.z1, MIN), math.min(step.z2, MAX) do
                grid[x][y][z] = step.state
            end
        end
    end
end

-- Count the number of cubes that are on
local function count_cubes_on(grid)
    local count = 0
    for x = MIN, MAX do
        for y = MIN, MAX do
            for z = MIN, MAX do
                if grid[x][y][z] then
                    count = count + 1
                end
            end
        end
    end
    return count
end

-- Main function to execute the reboot process
local function reboot_reactor(filename)
    local steps = read_input(filename)
    local grid = create_grid()
    for _, step in ipairs(steps) do
        apply_step(grid, step)
    end
    return count_cubes_on(grid)
end

-- Read the file and reboot the reactor
local total_on = reboot_reactor("input.txt")
print("Total cubes on:", total_on)