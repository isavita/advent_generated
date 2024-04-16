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
        elseif programs[i] == B then
            indexB = i
        end
    end
    exchange(programs, indexA, indexB)
end

local file = io.open("input.txt", "r")
local moves = {}
if file then
    for move in file:read("*l"):gmatch("[^,]+") do
        table.insert(moves, move)
    end
    file:close()
end

local programs = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p'}

for _, move in ipairs(moves) do
    local command = move:sub(1, 1)
    if command == 's' then
        local x = tonumber(move:sub(2))
        spin(programs, x)
    elseif command == 'x' then
        local pos = {}
        for p in move:sub(2):gmatch("[^/]+") do
            table.insert(pos, tonumber(p))
        end
        exchange(programs, pos[1] + 1, pos[2] + 1)
    elseif command == 'p' then
        local pos = {}
        for p in move:sub(2):gmatch("[^/]+") do
            table.insert(pos, p)
        end
        partner(programs, pos[1], pos[2])
    end
end

print(table.concat(programs))