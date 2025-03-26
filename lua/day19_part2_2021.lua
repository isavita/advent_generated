
-- Helper function to split a string
local function split(str, sep)
  local result = {}
  local pattern = string.format("([^%s]+)", sep)
  string.gsub(str, pattern, function(c) result[#result + 1] = c end)
  return result
end

-- Helper function to convert a point table to a string key
local function point_to_string(p)
  return string.format("%d,%d,%d", p[1], p[2], p[3])
end

-- Helper function to parse a string key back to a point table
local function string_to_point(s)
    local parts = split(s, ",")
    return { tonumber(parts[1]), tonumber(parts[2]), tonumber(parts[3]) }
end


local function read_input(filename)
  local scanners = {}
  local file = io.open(filename, 'r')
  if not file then
    error("Could not open file: " .. filename)
  end

  local current_scanner = nil
  for line in file:lines() do
    line = line:match("^%s*(.-)%s*$") -- trim whitespace
    if line:match("--- scanner") then
      if current_scanner then
        scanners[#scanners + 1] = current_scanner
      end
      current_scanner = {}
    elseif #line > 0 then
      local parts = split(line, ",")
      local point = { tonumber(parts[1]), tonumber(parts[2]), tonumber(parts[3]) }
      current_scanner[#current_scanner + 1] = point
    end
  end
  if current_scanner then
    scanners[#scanners + 1] = current_scanner
  end

  file:close()
  return scanners
end

local function get_rotations()
  -- Directly define the 24 rotation functions
  -- Based on mapping (x,y,z) to permutations of (+-x, +-y, +-z)
  return {
    function(p) return { p[1],  p[2],  p[3]} end,
    function(p) return { p[1], -p[2], -p[3]} end,
    function(p) return { p[1],  p[3], -p[2]} end,
    function(p) return { p[1], -p[3],  p[2]} end,

    function(p) return {-p[1], -p[2],  p[3]} end,
    function(p) return {-p[1],  p[2], -p[3]} end,
    function(p) return {-p[1], -p[3], -p[2]} end,
    function(p) return {-p[1],  p[3],  p[2]} end,

    function(p) return { p[2], -p[1],  p[3]} end,
    function(p) return { p[2],  p[1], -p[3]} end,
    function(p) return { p[2],  p[3],  p[1]} end,
    function(p) return { p[2], -p[3], -p[1]} end,

    function(p) return {-p[2],  p[1],  p[3]} end,
    function(p) return {-p[2], -p[1], -p[3]} end,
    function(p) return {-p[2], -p[3],  p[1]} end,
    function(p) return {-p[2],  p[3], -p[1]} end,

    function(p) return { p[3],  p[1],  p[2]} end,
    function(p) return { p[3], -p[1], -p[2]} end,
    function(p) return { p[3],  p[2], -p[1]} end,
    function(p) return { p[3], -p[2],  p[1]} end,

    function(p) return {-p[3], -p[1],  p[2]} end,
    function(p) return {-p[3],  p[1], -p[2]} end,
    function(p) return {-p[3], -p[2], -p[1]} end,
    function(p) return {-p[3],  p[2],  p[1]} end,
  }
end

local function add(p1, p2)
  return { p1[1] + p2[1], p1[2] + p2[2], p1[3] + p2[3] }
end

local function subtract(p1, p2)
  return { p1[1] - p2[1], p1[2] - p2[2], p1[3] - p2[3] }
end

local function manhattan(p1, p2)
  return math.abs(p1[1] - p2[1]) + math.abs(p1[2] - p2[2]) + math.abs(p1[3] - p2[3])
end

local function solve(scanners)
  local rotations = get_rotations()
  local num_scanners = #scanners

  local aligned = {} -- Use table as set, indices are 1-based
  aligned[1] = true

  local scanner_positions = {} -- Map scanner index (1-based) to position
  scanner_positions[1] = { 0, 0, 0 }

  local beacons = {} -- Use table as set, keys are point_to_string(p), values = p
  for _, p in ipairs(scanners[1]) do
    beacons[point_to_string(p)] = p
  end

  local pending = {} -- Use table as set, indices are 1-based
  for i = 2, num_scanners do
    pending[i] = true
  end

  while next(pending) do
    local found_match_in_iteration = false
    local scanner_to_remove = nil

    for scanner_idx in pairs(pending) do
      local scanner_data = scanners[scanner_idx]
      local best_match = nil -- { delta_point, count, rotated_beacons }

      for _, rot in ipairs(rotations) do
        local rotated = {}
        for i = 1, #scanner_data do
            rotated[i] = rot(scanner_data[i])
        end

        local deltas = {} -- key = point_to_string(delta), value = count

        for _, rotated_beacon in ipairs(rotated) do
          for _, aligned_beacon in pairs(beacons) do -- Iterate through values (points)
            local delta = subtract(aligned_beacon, rotated_beacon)
            local delta_key = point_to_string(delta)
            deltas[delta_key] = (deltas[delta_key] or 0) + 1
          end
        end

        local max_count = 0
        local max_delta_key = nil
        for delta_key, count in pairs(deltas) do
          if count > max_count then
            max_count = count
            max_delta_key = delta_key
          end
        end

        if max_count >= 12 then
          local max_delta_point = string_to_point(max_delta_key)
          best_match = { delta = max_delta_point, rotated = rotated }
          break -- Found orientation for this scanner
        end
      end -- end rotations loop

      if best_match then
        -- Align this scanner
        scanner_positions[scanner_idx] = best_match.delta
        for _, rotated_beacon in ipairs(best_match.rotated) do
          local final_beacon = add(rotated_beacon, best_match.delta)
          local final_beacon_key = point_to_string(final_beacon)
          beacons[final_beacon_key] = final_beacon
        end

        aligned[scanner_idx] = true
        scanner_to_remove = scanner_idx -- Mark for removal after iteration
        found_match_in_iteration = true
        break -- Found one alignment, restart the outer loop check
      end
    end -- end pending scanners loop

    if scanner_to_remove then
        pending[scanner_to_remove] = nil -- Remove the aligned scanner
    end

    -- Optimization: if no match found in a full pass, we might be stuck
    if not found_match_in_iteration and next(pending) then
        -- This case should ideally not happen with valid input if all scanners overlap
        -- If it does, it implies no further alignments are possible
        print("Warning: No alignment found for remaining scanners in an iteration.")
        break
    end

  end -- end while pending

  -- Calculate max Manhattan distance between scanner positions
  local positions_list = {}
  for i = 1, num_scanners do
      if scanner_positions[i] then
          positions_list[#positions_list + 1] = scanner_positions[i]
      end
  end


  local max_distance = 0
  for i = 1, #positions_list do
    for j = i + 1, #positions_list do
      local dist = manhattan(positions_list[i], positions_list[j])
      if dist > max_distance then
        max_distance = dist
      end
    end
  end

  return max_distance
end

local function main()
  local scanners = read_input('input.txt')
  local result = solve(scanners)
  print(result)
end

main()
