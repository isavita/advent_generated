local function parse_json(str, i)
    i = i or 1
    local char = str:sub(i, i)
    
    if char == '{' then
        local obj = {}
        local key, value
        i = i + 1
        while i <= #str do
            char = str:sub(i, i)
            if char == '}' then
                return obj, i + 1
            elseif char == '"' then
                key, i = parse_json(str, i)
                i = i + 1  -- Skip ':'
                value, i = parse_json(str, i)
                obj[key] = value
            elseif char == ',' then
                i = i + 1
            else
                i = i + 1
            end
        end
    elseif char == '[' then
        local arr = {}
        i = i + 1
        while i <= #str do
            char = str:sub(i, i)
            if char == ']' then
                return arr, i + 1
            elseif char ~= ',' then
                local value
                value, i = parse_json(str, i)
                table.insert(arr, value)
            else
                i = i + 1
            end
        end
    elseif char == '"' then
        local j = i + 1
        while j <= #str do
            if str:sub(j, j) == '"' and str:sub(j-1, j-1) ~= '\\' then
                return str:sub(i+1, j-1), j + 1
            end
            j = j + 1
        end
    else
        local j = i
        while j <= #str and str:sub(j, j):match('[0-9%-]') do
            j = j + 1
        end
        return tonumber(str:sub(i, j-1)), j
    end
end

local function sum_numbers(data)
    local total = 0
    
    if type(data) == "number" then
        return data
    elseif type(data) == "table" then
        if data[1] ~= nil then  -- It's an array
            for _, value in ipairs(data) do
                total = total + sum_numbers(value)
            end
        else  -- It's an object
            local has_red = false
            for _, value in pairs(data) do
                if value == "red" then
                    has_red = true
                    break
                end
            end
            if not has_red then
                for _, value in pairs(data) do
                    total = total + sum_numbers(value)
                end
            end
        end
    end
    
    return total
end

-- Read input from file
local file = io.open("input.txt", "r")
if not file then
    print("Error: Unable to open input.txt")
    os.exit(1)
end

local content = file:read("*all")
file:close()

-- Parse JSON and calculate sum
local parsed_data = parse_json(content)
local result = sum_numbers(parsed_data)

-- Print result
print(result)
