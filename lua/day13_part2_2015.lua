function runCode()
    function readHappinessValues(filename)
        local file = io.open(filename, "r")
        if not file then
            return nil, "Error opening file"
        end

        local happinessMap = {}
        for line in file:lines() do
            local parts = {}
            for part in string.gmatch(line, "%S+") do
                table.insert(parts, part)
            end

            if #parts < 11 then
                goto continue
            end

            local from = parts[1]
            local to = string.sub(parts[11], 1, -2) -- Trim period
            local change = tonumber(parts[4])
            if parts[3] == "lose" then
                change = -change
            end

            if not happinessMap[from] then
                happinessMap[from] = {}
            end
            happinessMap[from][to] = change

            ::continue::
        end

        file:close()
        return happinessMap
    end

    function addYourself(happinessMap)
        happinessMap["You"] = {}
        for guest, _ in pairs(happinessMap) do
            happinessMap[guest]["You"] = 0
            happinessMap["You"][guest] = 0
        end
    end

    function getGuestList(happinessMap)
        local guests = {}
        for guest, _ in pairs(happinessMap) do
            table.insert(guests, guest)
        end
        return guests
    end

    function calculateOptimalArrangement(guests, happinessMap)
        local maxHappiness = {0}
        permute(guests, 1, maxHappiness, happinessMap)
        return maxHappiness[1]
    end

    function permute(arr, i, maxHappiness, happinessMap)
        if i > #arr then
            return
        end
        if i == #arr then
            local happiness = calculateHappiness(arr, happinessMap)
            if happiness > maxHappiness[1] then
                maxHappiness[1] = happiness
            end
            return
        end
        for j = i, #arr do
            arr[i], arr[j] = arr[j], arr[i]
            permute(arr, i + 1, maxHappiness, happinessMap)
            arr[i], arr[j] = arr[j], arr[i]
        end
    end

    function calculateHappiness(arrangement, happinessMap)
        local happiness = 0
        local n = #arrangement
        for i = 1, n do
            local left = (i + n - 2) % n + 1
            local right = i % n + 1
            happiness = happiness + happinessMap[arrangement[i]][arrangement[left]]
            happiness = happiness + happinessMap[arrangement[i]][arrangement[right]]
        end
        return happiness
    end

    local happinessMap, err = readHappinessValues("input.txt")
    if not happinessMap then
        print("Error reading input:", err)
        return
    end

    addYourself(happinessMap)
    local guests = getGuestList(happinessMap)
    local maxHappiness = calculateOptimalArrangement(guests, happinessMap)
    print(maxHappiness)
end

runCode()