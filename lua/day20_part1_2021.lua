
function read_input(filename)
    local algorithm = ""
    local image = {}
    local is_algorithm = true

    for line in io.lines(filename) do
        if line == "" then
            is_algorithm = false
        elseif is_algorithm then
            algorithm = algorithm .. line
        else
            local row = {}
            for char in line:gmatch(".") do
                table.insert(row, char)
            end
            table.insert(image, row)
        end
    end

    return algorithm, image
end

function pad_image(image, padding, background)
    local padded = {}
    for _ = 1, padding do
        local empty_row = {}
        for _ = 1, #image[1] + 2 * padding do
            table.insert(empty_row, background)
        end
        table.insert(padded, empty_row)
    end

    for _, row in ipairs(image) do
        local padded_row = {}
        for _ = 1, padding do
            table.insert(padded_row, background)
        end
        for _, pixel in ipairs(row) do
            table.insert(padded_row, pixel)
        end
        for _ = 1, padding do
            table.insert(padded_row, background)
        end
        table.insert(padded, padded_row)
    end

    for _ = 1, padding do
        local empty_row = {}
        for _ = 1, #image[1] + 2 * padding do
            table.insert(empty_row, background)
        end
        table.insert(padded, empty_row)
    end

    return padded
end

function enhance_image(image, algorithm)
    local enhanced = {}
    local background = image[1][1]

    for y = 2, #image - 1 do
        local row = {}
        for x = 2, #image[y] - 1 do
            local binary = ""
            for dy = -1, 1 do
                for dx = -1, 1 do
                    binary = binary .. (image[y+dy][x+dx] == '#' and '1' or '0')
                end
            end
            local index = tonumber(binary, 2) + 1
            row[x-1] = algorithm:sub(index, index)
        end
        enhanced[y-1] = row
    end

    return enhanced
end

function count_lit_pixels(image)
    local count = 0
    for _, row in ipairs(image) do
        for _, pixel in ipairs(row) do
            if pixel == '#' then
                count = count + 1
            end
        end
    end
    return count
end

function main()
    local algorithm, image = read_input("input.txt")
    
    local padded_image = pad_image(image, 50, '.')
    local background = '.'

    for i = 1, 2 do
        padded_image = pad_image(padded_image, 2, background)
        padded_image = enhance_image(padded_image, algorithm)
        background = algorithm:sub(background == '.' and 1 or 512, background == '.' and 1 or 512)
    end

    print(count_lit_pixels(padded_image))
end

main()
