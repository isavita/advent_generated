-- Function to read input from file
local function read_input()
    local file = io.open("input.txt", "r")
    if not file then error("Could not open input file") end
    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()
    return lines
end

-- Function to swap positions in a string
local function swap_position(str, x, y)
    local chars = {}
    for i = 1, #str do
        chars[i] = str:sub(i, i)
    end
    chars[x+1], chars[y+1] = chars[y+1], chars[x+1]
    return table.concat(chars)
end

-- Function to swap letters in a string
local function swap_letter(str, x, y)
    return str:gsub(x, '\0'):gsub(y, x):gsub('\0', y)
end

-- Function to rotate a string left
local function rotate_left(str, x)
    x = x % #str
    return str:sub(x + 1) .. str:sub(1, x)
end

-- Function to rotate a string right
local function rotate_right(str, x)
    return rotate_left(str, #str - x)
end

-- Function to rotate based on position of letter
local function rotate_based_on_letter(str, x)
    local index = str:find(x) - 1
    local rotations = 1 + index + (index >= 4 and 1 or 0)
    return rotate_right(str, rotations)
end

-- Function to reverse rotate based on position of letter
local function reverse_rotate_based_on_letter(str, x)
    for i = 0, #str - 1 do
        local test = rotate_left(str, i)
        if rotate_based_on_letter(test, x) == str then
            return test
        end
    end
    return str
end

-- Function to reverse a substring
local function reverse_positions(str, x, y)
    local prefix = str:sub(1, x)
    local reversed = str:sub(x+1, y+1):reverse()
    local suffix = str:sub(y+2)
    return prefix .. reversed .. suffix
end

-- Function to move a letter from one position to another
local function move_position(str, x, y)
    local char = str:sub(x+1, x+1)
    str = str:sub(1, x) .. str:sub(x+2)
    return str:sub(1, y) .. char .. str:sub(y+1)
end

-- Function to unscramble the password
local function unscramble_password(password, instructions)
    for i = #instructions, 1, -1 do
        local words = {}
        for word in instructions[i]:gmatch("%S+") do
            table.insert(words, word)
        end
        
        if words[1] == "swap" and words[2] == "position" then
            password = swap_position(password, tonumber(words[3]), tonumber(words[6]))
        elseif words[1] == "swap" and words[2] == "letter" then
            password = swap_letter(password, words[3], words[6])
        elseif words[1] == "rotate" and words[2] == "left" then
            password = rotate_right(password, tonumber(words[3]))
        elseif words[1] == "rotate" and words[2] == "right" then
            password = rotate_left(password, tonumber(words[3]))
        elseif words[1] == "rotate" and words[2] == "based" then
            password = reverse_rotate_based_on_letter(password, words[7])
        elseif words[1] == "reverse" then
            password = reverse_positions(password, tonumber(words[3]), tonumber(words[5]))
        elseif words[1] == "move" then
            password = move_position(password, tonumber(words[6]), tonumber(words[3]))
        end
    end
    return password
end

-- Main function
local function main()
    local instructions = read_input()
    local scrambled_password = "fbgdceah"
    local unscrambled = unscramble_password(scrambled_password, instructions)
    print(unscrambled)
end

main()
