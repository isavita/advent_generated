-- Read input from file
local function read_input()
    local file = io.open("input.txt", "r")
    if not file then error("Could not open input file") end
    local content = file:read("*all")
    file:close()
    return content:gsub("%s", "") -- Remove any whitespace
end

-- Split the input into layers
local function split_layers(input, width, height)
    local layers = {}
    local layer_size = width * height
    for i = 1, #input, layer_size do
        table.insert(layers, input:sub(i, i + layer_size - 1))
    end
    return layers
end

-- Decode the image
local function decode_image(layers, width, height)
    local image = {}
    for y = 1, height do
        image[y] = {}
        for x = 1, width do
            local pixel_index = (y - 1) * width + x
            for _, layer in ipairs(layers) do
                local pixel = layer:sub(pixel_index, pixel_index)
                if pixel ~= "2" then -- Not transparent
                    image[y][x] = pixel
                    break
                end
            end
        end
    end
    return image
end

-- Print the decoded image
local function print_image(image)
    for _, row in ipairs(image) do
        local line = ""
        for _, pixel in ipairs(row) do
            line = line .. (pixel == "1" and "█" or " ")
        end
        print(line)
    end
end

-- Main function
local function main()
    local input = read_input()
    local width, height = 25, 6 -- As specified in the problem
    local layers = split_layers(input, width, height)
    local decoded_image = decode_image(layers, width, height)
    print("Decoded message:")
    print_image(decoded_image)
end

main()
