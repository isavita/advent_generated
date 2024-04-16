-- Function to compute the HASH value for a given string
function compute_hash(input)
    local current_value = 0
    for i = 1, #input do
        local ascii_code = string.byte(input, i)
        current_value = (current_value + ascii_code) * 17 % 256
    end
    return current_value
end

-- Read the input from the file
local file = io.open("input.txt", "r")
local initialization_sequence = file:read("*all"):gsub("\n", "")
file:close()

-- Split the initialization sequence into steps
local steps = {}
for step in initialization_sequence:gmatch("[^,]+") do
    table.insert(steps, step)
end

-- Initialize the boxes
local boxes = {}
for i = 0, 255 do
    boxes[i] = {}
end

-- Process each step
local total_focusing_power = 0
for _, step in ipairs(steps) do
    local label, operation, value = step:match("([^=^-]+)([=-])(%d*)")
    local box_index = compute_hash(label)

    if operation == "=" then
        -- Insert or replace a lens
        local found = false
        for i, lens in ipairs(boxes[box_index]) do
            if lens.label == label then
                lens.focal_length = tonumber(value)
                found = true
                break
            end
        end
        if not found then
            table.insert(boxes[box_index], {label = label, focal_length = tonumber(value)})
        end
    elseif operation == "-" then
        -- Remove a lens
        for i, lens in ipairs(boxes[box_index]) do
            if lens.label == label then
                table.remove(boxes[box_index], i)
                break
            end
        end
    end
end

-- Calculate the focusing power of all lenses
for box_index, lenses in pairs(boxes) do
    for slot_index, lens in ipairs(lenses) do
        total_focusing_power = total_focusing_power + (box_index + 1) * slot_index * lens.focal_length
    end
end

-- Output the total focusing power
print("Total focusing power:", total_focusing_power)