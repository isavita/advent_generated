function read_input(filename)
    local file = io.open(filename, "r")
    local initialState = ""
    local rules = {}
    for line in file:lines() do
        if line:find("initial state") then
            initialState = line:match("initial state: (.+)$")
        elseif line:find("=>") then
            local pattern, result = line:match("(.....) => (.)")
            rules[pattern] = result
        end
    end
    file:close()
    return initialState, rules
end

function min_max_keys(state)
    local minKey, maxKey = nil, nil
    for k in pairs(state) do
        if not minKey or k < minKey then minKey = k end
        if not maxKey or k > maxKey then maxKey = k end
    end
    return minKey, maxKey
end

function state_pattern(state)
    local minPot, maxPot = min_max_keys(state)
    local pattern, sum = {}, 0
    for i = minPot, maxPot do
        if state[i] == '#' then
            table.insert(pattern, '#')
            sum = sum + i
        else
            table.insert(pattern, '.')
        end
    end
    return table.concat(pattern), sum
end

function simulate_generations(initialState, rules, totalGenerations)
    local state = {}
    for i = 1, #initialState do
        if initialState:sub(i, i) == '#' then
            state[i-1] = '#'
        end
    end

    local previousPattern, previousSum, offset = "", 0, 0
    for generation = 1, totalGenerations do
        local newState = {}
        local minPot, maxPot = min_max_keys(state)
        for i = minPot - 2, maxPot + 2 do
            local pattern = {}
            for j = i - 2, i + 2 do
                table.insert(pattern, state[j] == '#' and '#' or '.')
            end
            pattern = table.concat(pattern)
            if rules[pattern] == '#' then
                newState[i] = '#'
            end
        end
        state = newState

        local currentPattern, currentSum = state_pattern(state)
        if currentPattern == previousPattern then
            offset = currentSum - previousSum
            local remainingGenerations = totalGenerations - generation
            local finalSum = currentSum + offset * remainingGenerations
            print(finalSum)
            return
        end
        previousPattern = currentPattern
        previousSum = currentSum
    end
end

local initialState, rules = read_input("input.txt")
simulate_generations(initialState, rules, 50000000000)