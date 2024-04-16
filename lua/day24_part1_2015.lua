local function readFile(filename)
    local file = io.open(filename, "r")
    if not file then error("File not found") end
    local content = file:read("*all")
    file:close()
    return content
end

local function split(str, sep)
    local fields = {}
    local pattern = string.format("([^%s]+)", sep)
    str:gsub(pattern, function(c) fields[#fields+1] = c end)
    return fields
end

local function calculateBestQuantumEntanglement(packages, targetWeight)
    local bestQE = math.huge
    local bestLength = math.huge

    local function backtrack(index, currentWeight, currentQE, currentLength)
        if currentWeight == targetWeight then
            if currentLength < bestLength or (currentLength == bestLength and currentQE < bestQE) then
                bestQE = currentQE
                bestLength = currentLength
            end
            return
        elseif currentWeight > targetWeight or index > #packages then
            return
        end

        -- Include current package
        local newQE = currentQE * packages[index]
        if newQE < bestQE then -- Only continue if the QE is still promising
            backtrack(index + 1, currentWeight + packages[index], newQE, currentLength + 1)
        end

        -- Exclude current package
        backtrack(index + 1, currentWeight, currentQE, currentLength)
    end

    table.sort(packages, function(a, b) return a > b end) -- Sort descending for better early pruning
    backtrack(1, 0, 1, 0)

    return bestQE
end

local function main()
    local data = readFile("input.txt")
    local lines = split(data, "\n")
    local packages = {}
    local totalWeight = 0

    for _, line in ipairs(lines) do
        local weight = tonumber(line)
        if not weight then
            error("Invalid weight in input: " .. line)
        end
        table.insert(packages, weight)
        totalWeight = totalWeight + weight
    end

    if totalWeight % 3 ~= 0 then
        error("Total weight is not divisible by 3")
    end
    local targetWeight = totalWeight / 3
    local bestQE = calculateBestQuantumEntanglement(packages, targetWeight)
    print(bestQE)
end

main()