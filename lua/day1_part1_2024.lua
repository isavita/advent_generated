-- Function to calculate total distance
local function calculate_total_distance(filename)
    -- Open the input file
    local file = io.open(filename, "r")
    if not file then
        print("Error: Could not open file.")
        return
    end

    -- Read lines from the file and parse the left and right lists
    local left_list = {}
    local right_list = {}

    for line in file:lines() do
        local left, right = line:match("(%d+)%s+(%d+)")
        table.insert(left_list, tonumber(left))
        table.insert(right_list, tonumber(right))
    end

    -- Close the file
    file:close()

    -- Sort the two lists
    table.sort(left_list)
    table.sort(right_list)

    -- Calculate the total distance
    local total_distance = 0
    for i = 1, #left_list do
        total_distance = total_distance + math.abs(left_list[i] - right_list[i])
    end

    -- Print the total distance
    print("Total Distance:", total_distance)
end

-- Run the function with the input file
calculate_total_distance("input.txt")
