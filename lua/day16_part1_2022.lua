
local function parse_input(filename)
    local valves = {}
    local valve_names = {}
    
    for line in io.lines(filename) do
        local valve = line:match("Valve (%w+)")
        local rate = tonumber(line:match("rate=(%d+)"))
        local tunnels = {}
        
        for tunnel in line:gmatch("(%w+)%s*,?") do
            if tunnel ~= valve then
                table.insert(tunnels, tunnel)
            end
        end
        
        valves[valve] = {
            rate = rate,
            tunnels = tunnels,
            open = false
        }
        table.insert(valve_names, valve)
    end
    
    return valves, valve_names
end

local function calculate_distances(valves)
    local distances = {}
    
    for start, _ in pairs(valves) do
        distances[start] = {}
        for dest, _ in pairs(valves) do
            distances[start][dest] = math.huge
        end
        distances[start][start] = 0
    end
    
    for start, valve in pairs(valves) do
        for _, tunnel in ipairs(valve.tunnels) do
            distances[start][tunnel] = 1
        end
    end
    
    for k, _ in pairs(valves) do
        for i, _ in pairs(valves) do
            for j, _ in pairs(valves) do
                distances[i][j] = math.min(
                    distances[i][j],
                    distances[i][k] + distances[k][j]
                )
            end
        end
    end
    
    return distances
end

local function max_pressure(valves, distances, time_left, current, opened)
    local max_release = 0
    
    for valve, data in pairs(valves) do
        if not opened[valve] and data.rate > 0 then
            local remaining_time = time_left - distances[current][valve] - 1
            
            if remaining_time > 0 then
                local new_opened = {}
                for k, v in pairs(opened) do
                    new_opened[k] = v
                end
                new_opened[valve] = true
                
                local pressure = remaining_time * data.rate +
                    max_pressure(valves, distances, remaining_time, valve, new_opened)
                
                max_release = math.max(max_release, pressure)
            end
        end
    end
    
    return max_release
end

local function solve_volcano_puzzle(filename)
    local valves, valve_names = parse_input(filename)
    local distances = calculate_distances(valves)
    
    local result = max_pressure(valves, distances, 30, "AA", {})
    print("Maximum pressure released:", result)
end

solve_volcano_puzzle("input.txt")
