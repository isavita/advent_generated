local function split(str, delim)
    local result = {}
    local from = 1
    local delim_from, delim_to = str:find(delim, from)
    while delim_from do
        table.insert(result, str:sub(from, delim_from - 1))
        from = delim_to + 1
        delim_from, delim_to = str:find(delim, from)
    end
    table.insert(result, str:sub(from))
    return result
end

local function spin(programs, x)
    local n = #programs
    local temp = {}
    for i = 1, n do
        temp[i] = programs[i]
    end

    for i = 1, n do
        programs[(i + x - 1) % n + 1] = temp[i]
    end
end

local function exchange(programs, A, B)
    programs[A], programs[B] = programs[B], programs[A]
end

local function partner(programs, A, B)
    local indexA, indexB
    for i = 1, #programs do
        if programs[i] == A then
            indexA = i
        end
        if programs[i] == B then
            indexB = i
        end
    end
    exchange(programs, indexA, indexB)
end

local function main()
    local file = io.open("input.txt", "r")
    local moves = {}
    for move in file:read():gmatch("[^,]+") do
        table.insert(moves, move)
    end
    file:close()

    local programs = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p'}
    local initial = table.concat(programs)
    local cycleLen = 0

    for i = 1, 1000000000 do
        for _, move in ipairs(moves) do
            local moveType = move:sub(1, 1)
            if moveType == 's' then
                local x = tonumber(move:sub(2))
                spin(programs, x)
            elseif moveType == 'x' then
                local positions = {}
                for pos in move:sub(2):gmatch("%d+") do
                    table.insert(positions, tonumber(pos))
                end
                exchange(programs, positions[1] + 1, positions[2] + 1)
            elseif moveType == 'p' then
                local positions = split(move:sub(2), "/")
                local A = positions[1]
                local B = positions[2]
                partner(programs, A, B)
            end
        end

        if table.concat(programs) == initial then
            cycleLen = i
            break
        end
    end

    local finalState = {initial:match("(.)")}
    for i = 1, 1000000000 % cycleLen do
        for _, move in ipairs(moves) do
            local moveType = move:sub(1, 1)
            if moveType == 's' then
                local x = tonumber(move:sub(2))
                spin(programs, x)
            elseif moveType == 'x' then
                local positions = {}
                for pos in move:sub(2):gmatch("%d+") do
                    table.insert(positions, tonumber(pos))
                end
                exchange(programs, positions[1] + 1, positions[2] + 1)
            elseif moveType == 'p' then
                local positions = split(move:sub(2), "/")
                local A = positions[1]
                local B = positions[2]
                partner(programs, A, B)
            end
        end
    end

    print(table.concat(programs))
end

main()