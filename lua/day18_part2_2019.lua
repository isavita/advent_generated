
local io = require("io")
local string = require("string")
local table = require("table")
local math = require("math")

-- Simple Min-Heap Implementation
local Heap = {}
Heap.__index = Heap

function Heap.new()
  local h = {}
  setmetatable(h, Heap)
  h.data = {}
  return h
end

function Heap:_swap(i, j)
  local tmp = self.data[i]
  self.data[i] = self.data[j]
  self.data[j] = tmp
end

function Heap:_sift_up(k)
  while k > 1 do
    local p = math.floor(k / 2)
    if self.data[p][1] <= self.data[k][1] then break end
    self:_swap(p, k)
    k = p
  end
end

function Heap:_sift_down(k)
  local n = #self.data
  while true do
    local c1 = k * 2
    local c2 = k * 2 + 1
    local smallest = k
    if c1 <= n and self.data[c1][1] < self.data[smallest][1] then
      smallest = c1
    end
    if c2 <= n and self.data[c2][1] < self.data[smallest][1] then
      smallest = c2
    end
    if smallest == k then break end
    self:_swap(k, smallest)
    k = smallest
  end
end

function Heap:push(item)
  table.insert(self.data, item)
  self:_sift_up(#self.data)
end

function Heap:pop()
  if #self.data == 0 then return nil end
  local item = self.data[1]
  local last = table.remove(self.data)
  if #self.data > 0 then
    self.data[1] = last
    self:_sift_down(1)
  end
  return item
end

function Heap:is_empty()
    return #self.data == 0
end

-- String split utility
local function split(s, sep)
    local fields = {}
    local pattern = string.format("([^%s]+)", sep)
    string.gsub(s, pattern, function(c) fields[#fields+1] = c end)
    return fields
end

-- Bitmask helper
local function bitmask(keys_set_table)
    local mask = 0
    for k, _ in pairs(keys_set_table) do
        mask = mask | (1 << (string.byte(k) - string.byte('a')))
    end
    return mask
end

-- Count set bits
local function count_set_bits(n)
    local count = 0
    while n > 0 do
        n = n & (n - 1)
        count = count + 1
    end
    return count
end

-- Generate canonical string key for positions
local function positions_to_key(positions_table)
    local temp = {}
    for i = 1, #positions_table do
        temp[i] = positions_table[i]
    end
    table.sort(temp)
    return table.concat(temp, ",")
end

-- Main logic
local function main()
    local f = assert(io.open("input.txt", "r"))
    local original_map = {}
    for line in f:lines() do
        line = string.gsub(line, "\n", "")
        if #line > 0 then
            local row = {}
            for i = 1, #line do
                row[i] = string.sub(line, i, i)
            end
            table.insert(original_map, row)
        end
    end
    f:close()

    local height = #original_map
    local width = #(original_map[1] or {})

    local found = false
    local center_y, center_x
    for y = 2, height - 1 do
        for x = 2, width - 1 do
            if original_map[y][x] == '@' then
                if original_map[y-1][x] == '.' and original_map[y+1][x] == '.' and
                   original_map[y][x-1] == '.' and original_map[y][x+1] == '.' then
                    center_y, center_x = y, x
                    found = true
                    break
                end
            end
        end
        if found then break end
    end

    if not found then
        print("Error: Could not find the '@' symbol surrounded by open spaces.")
        return
    end

    original_map[center_y-1][center_x-1] = '@'; original_map[center_y-1][center_x] = '#'; original_map[center_y-1][center_x+1] = '@'
    original_map[center_y][center_x-1]   = '#'; original_map[center_y][center_x]   = '#'; original_map[center_y][center_x+1]   = '#'
    original_map[center_y+1][center_x-1] = '@'; original_map[center_y+1][center_x] = '#'; original_map[center_y+1][center_x+1] = '@'


    local robot_positions = {}
    local keys = {}
    local all_keys_set = {}
    local key_positions = {} -- Includes robots

    for y = 1, height do
        for x = 1, width do
            local cell = original_map[y][x]
            if cell == '@' then
                local id = string.format("@%d", #robot_positions)
                table.insert(robot_positions, {x=x, y=y})
                key_positions[id] = {x=x, y=y}
            elseif string.match(cell, "%l") then
                keys[cell] = {x=x, y=y}
                all_keys_set[cell] = true
                key_positions[cell] = {x=x, y=y}
            end
        end
    end
    local total_keys = 0
    for _ in pairs(all_keys_set) do total_keys = total_keys + 1 end


    local function key_bfs(start_key)
        local start_pos = key_positions[start_key]
        local queue = {{x = start_pos.x, y = start_pos.y, dist = 0, required_keys_mask = 0}}
        local visited = {}
        visited[start_pos.x .. "," .. start_pos.y] = 0
        local results = {}
        local head = 1

        while head <= #queue do
            local current = queue[head]
            head = head + 1
            local x, y, dist, required_keys_mask = current.x, current.y, current.dist, current.required_keys_mask

            local cell = original_map[y][x]

            if string.match(cell, "%l") and cell ~= string.lower(start_key) and (required_keys_mask & (1 << (string.byte(cell) - string.byte('a')))) == 0 then
                 if not results[cell] or results[cell][1] > dist then
                     results[cell] = {dist, required_keys_mask}
                 end
                 -- Note: Unlike Python BFS, this version doesn't add the found key to required_keys_mask immediately for further exploration in *this* BFS.
                 -- The required_keys_mask only accumulates door requirements. The Dijkstra handles collected keys.
            end

            for _, d in ipairs{{-1,0}, {1,0}, {0,-1}, {0,1}} do
                local dx, dy = d[1], d[2]
                local nx, ny = x + dx, y + dy

                if ny >= 1 and ny <= height and nx >= 1 and nx <= width then
                    local ncell = original_map[ny][nx]
                    if ncell ~= '#' then
                        local n_required_keys_mask = required_keys_mask
                        if string.match(ncell, "%u") then
                            n_required_keys_mask = required_keys_mask | (1 << (string.byte(string.lower(ncell)) - string.byte('a')))
                        end

                        local visited_key = nx .. "," .. ny
                        if not visited[visited_key] or visited[visited_key] > n_required_keys_mask then
                             visited[visited_key] = n_required_keys_mask
                             table.insert(queue, {x=nx, y=ny, dist=dist+1, required_keys_mask=n_required_keys_mask})
                        end
                    end
                end
            end
        end
        return results
    end

    local key_graph = {}
    for key_name, _ in pairs(key_positions) do
         key_graph[key_name] = key_bfs(key_name)
    end


    local function dijkstra()
        local initial_positions_table = {}
        for i = 0, #robot_positions - 1 do
            initial_positions_table[i+1] = string.format("@%d", i)
        end

        local initial_positions_key = positions_to_key(initial_positions_table)
        local heap = Heap.new()
        heap:push({0, initial_positions_key, 0}) -- {cost, positions_key, collected_keys_mask}
        local visited = {}
        visited[initial_positions_key .. ":0"] = 0

        while not heap:is_empty() do
            local item = heap:pop()
            local cost, positions_key, collected_keys_mask = item[1], item[2], item[3]

            local state_key = positions_key .. ":" .. collected_keys_mask

            if visited[state_key] and visited[state_key] < cost then
                 goto continue -- Lua equivalent of continue
            end

            if count_set_bits(collected_keys_mask) == total_keys then
                return cost
            end

            local current_positions = split(positions_key, ",")

            for i = 1, #current_positions do
                local pos = current_positions[i]
                if key_graph[pos] then
                    for key, data in pairs(key_graph[pos]) do
                         local dist, required_mask = data[1], data[2]
                         local key_bit = 1 << (string.byte(key) - string.byte('a'))

                         -- If key not collected and requirements met
                         if (collected_keys_mask & key_bit) == 0 and (required_mask & collected_keys_mask) == required_mask then
                              local new_positions_table = {}
                              for j=1, #current_positions do new_positions_table[j] = current_positions[j] end
                              new_positions_table[i] = key

                              local new_positions_key = positions_to_key(new_positions_table)
                              local new_collected_keys_mask = collected_keys_mask | key_bit
                              local new_cost = cost + dist
                              local new_state_key = new_positions_key .. ":" .. new_collected_keys_mask

                              if not visited[new_state_key] or new_cost < visited[new_state_key] then
                                   visited[new_state_key] = new_cost
                                   heap:push({new_cost, new_positions_key, new_collected_keys_mask})
                              end
                         end
                    end
                end
            end
            ::continue::
        end
        return nil -- Should not happen if a solution exists
    end

    local result = dijkstra()
    print(result)

end

main()

