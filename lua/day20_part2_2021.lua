
local function readInput(filename)
    local file = io.open(filename, "r")
    local algorithm = file:read("*line")
    file:read("*line")  -- skip empty line
    
    local image = {}
    for line in file:lines() do
        local row = {}
        for i = 1, #line do
            row[i] = line:sub(i,i) == '#'
        end
        table.insert(image, row)
    end
    file:close()
    return algorithm, image
end

local function enhanceImage(algorithm, image, useInfiniteLit)
    local expandBy = 1
    local newImage = {}
    local height = #image + (expandBy * 2)
    local width = #image[1] + (expandBy * 2)
    
    for y = 1, height do
        newImage[y] = {}
        for x = 1, width do
            local index = 0
            for dy = -1, 1 do
                for dx = -1, 1 do
                    index = index * 2
                    local ny, nx = y + dy - expandBy, x + dx - expandBy
                    
                    local bit = 0
                    if ny > 0 and ny <= #image and nx > 0 and nx <= #image[1] then
                        bit = image[ny][nx] and 1 or 0
                    elseif useInfiniteLit then
                        bit = 1
                    end
                    
                    index = index + bit
                end
            end
            
            newImage[y][x] = algorithm:sub(index + 1, index + 1) == '#'
        end
    end
    
    return newImage
end

local function countLitPixels(image)
    local count = 0
    for _, row in ipairs(image) do
        for _, pixel in ipairs(row) do
            if pixel then count = count + 1 end
        end
    end
    return count
end

local function main()
    local iterations = 50
    local algorithm, image = readInput("input.txt")
    
    for i = 1, iterations do
        image = enhanceImage(algorithm, image, i % 2 == 0 and algorithm:sub(1,1) == '#')
    end
    
    print(countLitPixels(image))
end

main()
