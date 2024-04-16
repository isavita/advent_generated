function read_all(file_path)
    local file = io.open(file_path, "r")
    if not file then return nil end
    local content = file:read("*all")
    file:close()
    return content
end

function abs(n)
    if n < 0 then
        return -n
    else
        return n
    end
end

function manhattan(x1, y1, x2, y2)
    return abs(x1 - x2) + abs(y1 - y2)
end

function impossible(sensors, y)
    local pts = {}
    for _, s in ipairs(sensors) do
        local dist = s.dist - abs(s.pos_y - y)
        if dist >= 0 then  -- Ensure dist is not negative
            for x = 0, dist do
                pts[s.pos_x + x] = true
                pts[s.pos_x - x] = true
            end
        end
    end
    for _, s in ipairs(sensors) do
        if s.beacon_y == y then
            pts[s.beacon_x] = nil
        end
    end
    local count = 0
    for _ in pairs(pts) do
        count = count + 1
    end
    return count
end

function main()
    local sensors = {}
    local input = read_all("input.txt")
    if not input then
        print("Failed to read input file.")
        return
    end
    for line in string.gmatch(input, "[^\n]+") do
        local px, py, bx, by = line:match("Sensor at x=(%d+), y=(%d+): closest beacon is at x=(%d+), y=(%d+)")
        if px and py and bx and by then
            local s = {
                pos_x = tonumber(px),
                pos_y = tonumber(py),
                beacon_x = tonumber(bx),
                beacon_y = tonumber(by),
                dist = manhattan(tonumber(px), tonumber(py), tonumber(bx), tonumber(by))
            }
            table.insert(sensors, s)
        else
            print("Failed to parse line:", line)
        end
    end
    print(impossible(sensors, 2000000))
end

main()