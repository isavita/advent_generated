local file = io.open("input.txt", "r")
if not file then
    print("Failed to open file")
    return
end

local replacements = {}
local molecule = nil

for line in file:lines() do
    if line == "" then
        -- Skip empty lines
    elseif string.find(line, " => ") then
        table.insert(replacements, line)
    else
        molecule = line
    end
end
file:close()

local molecules = {}
for _, replacement in ipairs(replacements) do
    local parts = {}
    for part in string.gmatch(replacement, "%S+") do
        table.insert(parts, part)
    end
    local from, to = parts[1], parts[3]
    local start = 1
    repeat
        local found_at = string.find(molecule, from, start, true)
        if found_at then
            local newMolecule = molecule:sub(1, found_at - 1) .. to .. molecule:sub(found_at + #from)
            molecules[newMolecule] = true
            start = found_at + 1
        end
    until not found_at
end

-- Count the number of unique molecules
local count = 0
for _ in pairs(molecules) do
    count = count + 1
end

print(count)