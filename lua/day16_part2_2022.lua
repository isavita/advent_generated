
local function max_pressure(valves, curr, minute, pressure, open_valves)
  local max_pressure_value = pressure
  for i = 1, #open_valves do
    local next_valve = open_valves[i]
    local time_left = minute - valves[curr].tunnels[next_valve] - 1
    if time_left > 0 then
      local new_open_valves = {}
      for j = 1, #open_valves do
        if j ~= i then
          table.insert(new_open_valves, open_valves[j])
        end
      end
      max_pressure_value = math.max(max_pressure_value, max_pressure(valves, next_valve, time_left, time_left * valves[next_valve].flow + pressure, new_open_valves))
    end
  end
  return max_pressure_value
end

local function divide(n)
  if n == 1 then
    return {
      { {}, { 1 } },
      { { 1 }, {} }
    }
  end

  local prev_divisions = divide(n - 1)
  local result = {}

  for i = 1, #prev_divisions do
    local mine = { table.unpack(prev_divisions[i][1]) }
    table.insert(mine, n)
    table.insert(result, { mine, prev_divisions[i][2] })
  end

  for i = 1, #prev_divisions do
    local elephant = { table.unpack(prev_divisions[i][2]) }
    table.insert(elephant, n)
    table.insert(result, { prev_divisions[i][1], elephant })
  end

  return result
end

local valves = {}
local open_valves_list = {}

for line in io.lines("input.txt") do
  local valve_id, flow_rate_str, tunnels_str = string.match(line, "Valve (%w+) has flow rate=(%d+); tunnels? leads? to valves? (.*)")
  local flow_rate = tonumber(flow_rate_str)

  valves[valve_id] = {
    id = valve_id,
    flow = flow_rate,
    tunnels = {}
  }

  if flow_rate > 0 then
    table.insert(open_valves_list, valve_id)
  end

  for tunnel in string.gmatch(tunnels_str, "(%w+)") do
    valves[valve_id].tunnels[tunnel] = 1
  end
  valves[valve_id].tunnels[valve_id] = 0
end

for k, _ in pairs(valves) do
  for i, _ in pairs(valves) do
    for j, _ in pairs(valves) do
      if valves[i].tunnels[k] and valves[k].tunnels[j] then
        if not valves[i].tunnels[j] or valves[i].tunnels[j] > valves[i].tunnels[k] + valves[k].tunnels[j] then
          valves[i].tunnels[j] = valves[i].tunnels[k] + valves[k].tunnels[j]
        end
      end
    end
  end
end

local max_total_pressure_value = 0
local divisions = divide( #open_valves_list )

for i = 1, #divisions do
  local d = divisions[i]
  if #d[1] > 0 and #d[2] > 0 then
    local mine = {}
    for _, index in ipairs(d[1]) do
      table.insert(mine, open_valves_list[index])
    end

    local elephant = {}
    for _, index in ipairs(d[2]) do
      table.insert(elephant, open_valves_list[index])
    end

    local total_pressure = max_pressure(valves, "AA", 26, 0, mine) + max_pressure(valves, "AA", 26, 0, elephant)
    max_total_pressure_value = math.max(max_total_pressure_value, total_pressure)
  end
end

print(max_total_pressure_value)
