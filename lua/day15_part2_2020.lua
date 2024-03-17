local function read_file(filename)
    local file = io.open(filename, "r")
    if not file then
        error("Error reading file: " .. filename)
    end
    local content = file:read("*a")
    file:close()
    return content
end

local function to_number(str)
    return tonumber(str)
end

local function main()
    local data = read_file("input.txt")
    local starting_numbers = {}
    for num in string.gmatch(data, "%d+") do
        table.insert(starting_numbers, to_number(num))
    end

    local spoken = {}
    local last_spoken = 0
    for i, number in ipairs(starting_numbers) do
        if i == #starting_numbers then
            last_spoken = number
        else
            spoken[number] = i
        end
    end

    for turn = #starting_numbers + 1, 30000000 do
        local next_number = 0
        if spoken[last_spoken] then
            next_number = turn - 1 - spoken[last_spoken]
        end
        spoken[last_spoken] = turn - 1
        last_spoken = next_number
    end

    print(last_spoken)
end

main()