
function read_file(path)
    local file = io.open(path, "r")
    local content = file:read("*all")
    file:close()
    return content
end

function split_lines(input)
    local lines = {}
    for line in input:gmatch("[^\r\n]+") do
        table.insert(lines, line)
    end
    return lines
end

function split_values(line)
    local values = {}
    for value in line:gmatch("%d+") do
        table.insert(values, tonumber(value))
    end
    return values
end

function min(a, b)
    if a < b then
        return a
    else
        return b
    end
end

function max(a, b)
    if a > b then
        return a
    else
        return b
    end
end

function add_points(p1, p2)
    return {p1[1] + p2[1], p1[2] + p2[2], p1[3] + p2[3]}
end

function main()
    local content = read_file("input.txt")
    local lines = split_lines(content)

    local cubes = {}
    local neighbors = {
        {-1, 0, 0},
        {1, 0, 0},
        {0, -1, 0},
        {0, 1, 0},
        {0, 0, -1},
        {0, 0, 1}
    }
    local min_point = {math.huge, math.huge, math.huge}
    local max_point = {-math.huge, -math.huge, -math.huge}

    for _, line in ipairs(lines) do
        if line ~= "" then
            local values = split_values(line)
            local cube = {values[1], values[2], values[3]}
            cubes[table.concat(cube, ",")] = true
            min_point = {min(min_point[1], cube[1]), min(min_point[2], cube[2]), min(min_point[3], cube[3])}
            max_point = {max(max_point[1], cube[1]), max(max_point[2], cube[2]), max(max_point[3], cube[3])}
        end
    end

    min_point = add_points(min_point, {-1, -1, -1})
    max_point = add_points(max_point, {1, 1, 1})

    local faces = 0
    local q = {min_point}
    local seen = {[table.concat(min_point, ",")] = true}

    while #q > 0 do
        local curr = table.remove(q, 1)
        for _, delta in ipairs(neighbors) do
            local next = add_points(curr, delta)
            if next[1] < min_point[1] or next[2] < min_point[2] or next[3] < min_point[3] or
                next[1] > max_point[1] or next[2] > max_point[2] or next[3] > max_point[3] then
                goto continue
            end
            if cubes[table.concat(next, ",")] then
                faces = faces + 1
            elseif not seen[table.concat(next, ",")] then
                seen[table.concat(next, ",")] = true
                table.insert(q, next)
            end
            ::continue::
        end
    end

    print(faces)
end

main()
