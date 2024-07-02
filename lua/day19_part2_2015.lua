math.randomseed(os.time())  -- Initialize random seed

-- Function to read input from file
local function read_input(filename)
    local file = io.open(filename, "r")
    if not file then
        print("Error: Unable to open input file")
        os.exit(1)
    end

    local replacements = {}
    local molecule = ""

    for line in file:lines() do
        if line:find("=>") then
            local from, to = line:match("(%w+) => (%w+)")
            table.insert(replacements, {from = from, to = to})
        elseif #line > 0 then
            molecule = line
        end
    end

    file:close()
    return replacements, molecule
end

-- Function to shuffle a table
local function shuffle(tbl)
    for i = #tbl, 2, -1 do
        local j = math.random(i)
        tbl[i], tbl[j] = tbl[j], tbl[i]
    end
end

-- Function to find the number of steps to reduce molecule to 'e'
local function reduce_to_e(replacements, molecule)
    local steps = 0
    local current = molecule

    while current ~= "e" do
        local initial = current
        shuffle(replacements)  -- Randomize the order of replacements

        for _, replacement in ipairs(replacements) do
            local from, to = replacement.from, replacement.to
            local i, j = current:find(to)
            if i then
                current = current:sub(1, i-1) .. from .. current:sub(j+1)
                steps = steps + 1
                break
            end
        end

        if current == initial then
            -- If we couldn't make any replacements, start over
            return nil
        end
    end

    return steps
end

-- Main execution
local replacements, molecule = read_input("input.txt")

local steps
local attempts = 0
repeat
    steps = reduce_to_e(replacements, molecule)
    attempts = attempts + 1
    if attempts % 100 == 0 then
        print("Attempts: " .. attempts)
    end
until steps or attempts >= 1000

if steps then
    print("Fewest number of steps to produce the medicine molecule:", steps)
else
    print("Failed to produce the medicine molecule after 1000 attempts")
end
