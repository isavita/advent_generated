local md5 = require("md5")

-- Read the salt from input.txt
local function read_input()
    local file = io.open("input.txt", "r")
    if not file then
        error("Could not open input.txt")
    end
    local salt = file:read("*l")
    file:close()
    return salt
end

-- Check if a string contains a triplet
local function has_triplet(str)
    for i = 1, #str - 2 do
        if str:sub(i, i) == str:sub(i+1, i+1) and str:sub(i, i) == str:sub(i+2, i+2) then
            return str:sub(i, i)
        end
    end
    return nil
end

-- Check if a string contains five consecutive occurrences of a character
local function has_quintuplet(str, char)
    return str:find(char:rep(5))
end

-- Generate stretched MD5 hash
local function generate_stretched_hash(salt, index)
    local hash = md5.sumhexa(salt .. index)
    for _ = 1, 2016 do
        hash = md5.sumhexa(hash)
    end
    return hash
end

-- Main function to solve the problem
local function solve()
    local salt = read_input()
    local keys_found = 0
    local index = 0
    local cache = {}

    while keys_found < 64 do
        local hash = cache[index] or generate_stretched_hash(salt, index)
        cache[index] = hash
        local triplet = has_triplet(hash)

        if triplet then
            for i = 1, 1000 do
                local next_index = index + i
                local next_hash = cache[next_index] or generate_stretched_hash(salt, next_index)
                cache[next_index] = next_hash

                if has_quintuplet(next_hash, triplet) then
                    keys_found = keys_found + 1
                    if keys_found == 64 then
                        return index
                    end
                    break
                end
            end
        end

        index = index + 1
    end
end

-- Run the solver and print the result
local result = solve()
print(result)
