
local function solve()
    local lines = {}
    for line in io.lines("input.txt") do
        if line ~= "" then
            table.insert(lines, line)
        end
    end

    local function parseLine(line)
        local parts = {}
        for part in line:gmatch("%S+") do
            table.insert(parts, part)
        end
        local springs = parts[1]
        local groupStr = parts[2]
        local group = {}
        for numStr in groupStr:gmatch("([^,]+)") do
            table.insert(group, tonumber(numStr))
        end
        return {springs = springs, group = group}
    end

    local rows = {}
    for _, line in ipairs(lines) do
        table.insert(rows, parseLine(line))
    end

    local function unfoldRow(row, factor)
        local newSprings = row.springs
        local newGroup = {}
        for _ = 1, factor do
            for _, num in ipairs(row.group) do
                table.insert(newGroup, num)
            end
        end
        for i = 1, factor - 1 do
            newSprings = newSprings .. "?" .. row.springs
        end
        return {springs = newSprings, group = newGroup}
    end

    local function countArrangements(row)
        local springs = row.springs
        local group = row.group
        local n = #springs
        local m = #group
        local memo = {}

        local function recurse(iSprings, iGroup, iContiguous)
            local key = iSprings .. "_" .. iGroup .. "_" .. iContiguous
            if memo[key] ~= nil then
                return memo[key]
            end

            if iSprings == n then
                if iGroup > m and iContiguous == 0 then
                    memo[key] = 1
                    return 1
                elseif iGroup == m and iContiguous == group[m] then
                    memo[key] = 1
                    return 1
                else
                    memo[key] = 0
                    return 0
                end
            end

            local c = springs:sub(iSprings + 1, iSprings + 1)
            local total = 0

            if c == '.' or c == '?' then
                if iContiguous == 0 then
                    total = total + recurse(iSprings + 1, iGroup, 0)
                elseif iContiguous == group[iGroup] then
                    total = total + recurse(iSprings + 1, iGroup + 1, 0)
                end
            end

            if c == '#' or c == '?' then
                if iGroup <= m and iContiguous < group[iGroup] then
                    total = total + recurse(iSprings + 1, iGroup, iContiguous + 1)
                end
            end

            memo[key] = total
            return total
        end

        return recurse(0, 1, 0)
    end

    local total = 0
    for _, row in ipairs(rows) do
        local unfolded = unfoldRow(row, 5)
        total = total + countArrangements(unfolded)
    end

    print(total)
end

solve()
