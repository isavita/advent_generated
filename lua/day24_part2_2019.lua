
local Side = 5
local Square = Side * Side

local function parse()
    local res = {}
    local file = io.open("input.txt", "r")
    for row = 0, Side-1 do
        local line = file:read("*line")
        for col = 0, Side-1 do
            res[row*Side + col] = line:sub(col+1,col+1) == '#'
        end
    end
    file:close()
    return res
end

local function infested(space, level, cell)
    local grid = space[level]
    return grid and grid[cell] or false
end

local function minMaxLevel(space)
    local min, max = math.huge, -math.huge
    for level in pairs(space) do
        min = math.min(min, level)
        max = math.max(max, level)
    end
    return min, max
end

local function clean(space)
    local min, max = minMaxLevel(space)
    local countMin, countMax = 0, 0
    
    for cell = 0, Square-1 do
        if space[min] and space[min][cell] then countMin = countMin + 1 end
        if space[max] and space[max][cell] then countMax = countMax + 1 end
    end
    
    if countMin == 0 then space[min] = nil end
    if countMax == 0 then space[max] = nil end
end

local function next_generation(space)
    local newSpace = {}
    local minLevel, maxLevel = minMaxLevel(space)
    
    for level = minLevel - 1, maxLevel + 1 do
        newSpace[level] = {}
        
        for cell = 0, Square-1 do
            if cell ~= 12 then
                local row, col = math.floor(cell / Side), cell % Side
                local neighbours = 0
                
                -- Complex neighbor checking logic (similar to Go version)
                if row == 0 then
                    if infested(space, level-1, 7) then neighbours = neighbours + 1 end
                end
                
                if col == 0 then
                    if infested(space, level-1, 11) then neighbours = neighbours + 1 end
                end
                
                if col == 4 then
                    if infested(space, level-1, 13) then neighbours = neighbours + 1 end
                end
                
                if row == 4 then
                    if infested(space, level-1, 17) then neighbours = neighbours + 1 end
                end
                
                -- Additional neighbor checking for special cells
                if cell == 7 then
                    for i = 0, Side-1 do
                        if infested(space, level+1, i) then neighbours = neighbours + 1 end
                    end
                end
                
                if cell == 11 then
                    for i = 0, Side-1 do
                        if infested(space, level+1, 5*i) then neighbours = neighbours + 1 end
                    end
                end
                
                if cell == 13 then
                    for i = 0, Side-1 do
                        if infested(space, level+1, 5*i+Side-1) then neighbours = neighbours + 1 end
                    end
                end
                
                if cell == 17 then
                    for i = 0, Side-1 do
                        if infested(space, level+1, (Side-1)*Side+i) then neighbours = neighbours + 1 end
                    end
                end
                
                -- Standard neighbor checking
                if row > 0 and cell ~= 17 then
                    if infested(space, level, cell-Side) then neighbours = neighbours + 1 end
                end
                
                if col > 0 and cell ~= 13 then
                    if infested(space, level, cell-1) then neighbours = neighbours + 1 end
                end
                
                if col < Side-1 and cell ~= 11 then
                    if infested(space, level, cell+1) then neighbours = neighbours + 1 end
                end
                
                if row < Side-1 and cell ~= 7 then
                    if infested(space, level, cell+Side) then neighbours = neighbours + 1 end
                end
                
                -- Apply rules
                local current = infested(space, level, cell)
                if current and neighbours ~= 1 then
                    newSpace[level][cell] = false
                elseif not current and (neighbours == 1 or neighbours == 2) then
                    newSpace[level][cell] = true
                else
                    newSpace[level][cell] = current
                end
            end
        end
    end
    
    clean(newSpace)
    return newSpace
end

local function main()
    local input = parse()
    local space = {[0] = input}
    
    for _ = 1, 200 do
        space = next_generation(space)
    end
    
    local count = 0
    for _, grid in pairs(space) do
        for _, cell in pairs(grid) do
            if cell then count = count + 1 end
        end
    end
    
    print(count)
end

main()
