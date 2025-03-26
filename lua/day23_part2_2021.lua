
local Heap = {}
Heap.__index = Heap

function Heap.new(compare_func)
    local self = setmetatable({}, Heap)
    self.data = {}
    self.compare_func = compare_func or function(a, b) return a < b end
    return self
end

function Heap:push(item)
    table.insert(self.data, item)
    local index = #self.data
    while index > 1 do
        local parent_index = math.floor(index / 2)
        if self.compare_func(self.data[index], self.data[parent_index]) then
            self.data[index], self.data[parent_index] = self.data[parent_index], self.data[index]
            index = parent_index
        else
            break
        end
    end
end

function Heap:pop()
    if #self.data == 0 then
        return nil
    end
    local top = self.data[1]
    local last = table.remove(self.data)
    if #self.data > 0 then
        self.data[1] = last
        local index = 1
        while true do
            local left_child_index = index * 2
            local right_child_index = index * 2 + 1
            local smallest_index = index

            if left_child_index <= #self.data and self.compare_func(self.data[left_child_index], self.data[smallest_index]) then
                smallest_index = left_child_index
            end

            if right_child_index <= #self.data and self.compare_func(self.data[right_child_index], self.data[smallest_index]) then
                smallest_index = right_child_index
            end

            if smallest_index ~= index then
                self.data[index], self.data[smallest_index] = self.data[smallest_index], self.data[index]
                index = smallest_index
            else
                break
            end
        end
    end
    return top
end

function Heap:is_empty()
    return #self.data == 0
end

local State = {}
State.__index = State

function State.new(grid, energy_used, path)
    local self = setmetatable({}, State)
    self.grid = grid
    self.energy_used = energy_used
    self.path = path
    return self
end

function State:__lt(other)
    return self.energy_used < other.energy_used
end

function State:grid_to_string()
    local rows = {}
    for i = 1, #self.grid do
        table.insert(rows, table.concat(self.grid[i], ""))
    end
    return table.concat(rows, "\n")
end

function deep_copy_table(orig)
    local copy = {}
    for k, v in pairs(orig) do
        if type(v) == "table" then
            copy[k] = deep_copy_table(v)
        else
            copy[k] = v
        end
    end
    return copy
end

function State:copy()
    return State.new(deep_copy_table(self.grid), self.energy_used, self.path)
end

function State:is_all_done(room_coord_to_want_char)
    for coord_str, want in pairs(room_coord_to_want_char) do
         local parts = {}
         for part in string.gmatch(coord_str, "[^,]+") do table.insert(parts, tonumber(part)) end
         local r, c = parts[1], parts[2]
         if self.grid[r][c] ~= want then
            return false
        end
    end
    return true
end

function State:get_unsettled_coords(room_coord_to_want_char)
    local unsettled = {}
    local hallway_row = 2
    for col = 1, #self.grid[1] do
        if string.find("ABCD", self.grid[hallway_row][col], 1, true) then
            table.insert(unsettled, {hallway_row, col})
        end
    end

    local room_cols = {4, 6, 8, 10}
    for _, col in ipairs(room_cols) do
        local room_full_from_back = true
        for row = #self.grid - 1, 3, -1 do
            local coord = {row, col}
            local coord_str = row .. "," .. col
            local want_char = room_coord_to_want_char[coord_str]
            local got_char = self.grid[row][col]
            if got_char ~= "." then
                if got_char ~= want_char then
                    room_full_from_back = false
                    table.insert(unsettled, coord)
                elseif got_char == want_char and not room_full_from_back then
                    table.insert(unsettled, coord)
                end
            end
        end
    end
    return unsettled
end

function State:get_next_possible_moves(unsettled_coord, room_coord_to_want_char)
    local unsettled_char = self.grid[unsettled_coord[1]][unsettled_coord[2]]
    if not string.find("ABCD", unsettled_char, 1, true) then
        error("Unexpected character to get next moves for: " .. unsettled_char)
    end

    local possible = {}
    local started_in_hallway = unsettled_coord[1] == 2 -- Hallway row is 2

    local queue = {{unsettled_coord, 0}} -- {coord, dist}
    local visited = {}
    visited[unsettled_coord[1] .. "," .. unsettled_coord[2]] = 0

    local head = 1
    while head <= #queue do
        local current_entry = queue[head]
        head = head + 1
        local front = current_entry[1]
        local dist = current_entry[2]
        local front_str = front[1] .. "," .. front[2]

        if front[1] ~= unsettled_coord[1] or front[2] ~= unsettled_coord[2] then
             -- Check if target is valid
             local is_hallway_entrance = (front[1] == 2 and (front[2] == 4 or front[2] == 6 or front[2] == 8 or front[2] == 10))
             if not is_hallway_entrance then
                local want_char = room_coord_to_want_char[front_str]
                if want_char == nil then -- Moving into hallway
                    if not started_in_hallway then
                         table.insert(possible, front)
                    end
                elseif want_char == unsettled_char then -- Moving into target room
                    local is_stuck_amphipod = false
                    local room_has_deeper_open_spaces = false
                    for r = front[1] + 1, #self.grid - 1 do
                        local char = self.grid[r][front[2]]
                        if char == "." then
                            room_has_deeper_open_spaces = true
                        elseif char ~= "." and char ~= unsettled_char then
                            is_stuck_amphipod = true
                            break
                        end
                    end
                    if not room_has_deeper_open_spaces and not is_stuck_amphipod then
                        table.insert(possible, front)
                    end
                end
             end
        end

        local dr = {-1, 1, 0, 0}
        local dc = {0, 0, -1, 1}
        for i = 1, 4 do
            local next_row, next_col = front[1] + dr[i], front[2] + dc[i]
            local next_coord = {next_row, next_col}
            local next_coord_str = next_row .. "," .. next_col

            if next_row >= 1 and next_row <= #self.grid and
               next_col >= 1 and next_col <= #self.grid[1] and
               self.grid[next_row][next_col] == "." and
               (visited[next_coord_str] == nil or visited[next_coord_str] > dist + 1) then

                 visited[next_coord_str] = dist + 1
                 table.insert(queue, {next_coord, dist + 1})
            end
        end
    end

    return possible
end


function calc_energy(char, start_coord, end_coord)
    local dist = math.abs(end_coord[2] - start_coord[2])
    dist = dist + (start_coord[1] - 2) -- Hallway is row 2
    dist = dist + (end_coord[1] - 2)   -- Hallway is row 2

    local energy_per_type = {A = 1, B = 10, C = 100, D = 1000}
    if not energy_per_type[char] then
        error("Unexpected character: " .. char)
    end
    return energy_per_type[char] * dist
end

function amphipod(input_str)
    local grid_lines = {}
    for line in string.gmatch(input_str, "[^\r\n]+") do
        local row = {}
        for i = 1, #line do
            table.insert(row, string.sub(line, i, i))
        end
        table.insert(grid_lines, row)
    end

    local start_grid = grid_lines

    local insert1_chars = {}
    for c in string.gmatch("  #D#C#B#A#  ", ".") do table.insert(insert1_chars, c) end
    local insert2_chars = {}
    for c in string.gmatch("  #D#B#A#C#  ", ".") do table.insert(insert2_chars, c) end

    table.insert(start_grid, 4, insert1_chars)
    table.insert(start_grid, 5, insert2_chars)

    local start_state = State.new(start_grid, 0, "")

    -- Adjust room coords for 1-based indexing and inserted rows
    local room_coord_to_want_char = {
        ["3,4"] = "A", ["4,4"] = "A", ["5,4"] = "A", ["6,4"] = "A",
        ["3,6"] = "B", ["4,6"] = "B", ["5,6"] = "B", ["6,6"] = "B",
        ["3,8"] = "C", ["4,8"] = "C", ["5,8"] = "C", ["6,8"] = "C",
        ["3,10"] = "D", ["4,10"] = "D", ["5,10"] = "D", ["6,10"] = "D"
    }

    local min_heap = Heap.new(function(a, b) return a.energy_used < b.energy_used end)
    min_heap:push(start_state)
    local seen_grids = {}

    while not min_heap:is_empty() do
        local front = min_heap:pop()
        local key = front:grid_to_string()

        if seen_grids[key] then
            goto continue
        end
        seen_grids[key] = true

        if front:is_all_done(room_coord_to_want_char) then
            return front.energy_used
        end

        local unsettled_coords = front:get_unsettled_coords(room_coord_to_want_char)
        for _, unsettled_coord in ipairs(unsettled_coords) do
            local next_moves = front:get_next_possible_moves(unsettled_coord, room_coord_to_want_char)
            for _, next_coord in ipairs(next_moves) do
                 if front.grid[next_coord[1]][next_coord[2]] ~= "." then
                     error("Should only be moving to walkable spaces, got " .. front.grid[next_coord[1]][next_coord[2]] .. " at " .. next_coord[1] .. "," .. next_coord[2])
                 end

                 local cp = front:copy()
                 local moving_char = cp.grid[unsettled_coord[1]][unsettled_coord[2]]
                 cp.energy_used = cp.energy_used + calc_energy(moving_char, unsettled_coord, next_coord)
                 -- cp.path = cp.path .. string.format("%s{%d,%d}->{%d,%d}(%d),", moving_char, unsettled_coord[1], unsettled_coord[2], next_coord[1], next_coord[2], cp.energy_used) -- Path tracking disabled for brevity
                 cp.grid[next_coord[1]][next_coord[2]] = moving_char
                 cp.grid[unsettled_coord[1]][unsettled_coord[2]] = "."
                 min_heap:push(cp)
            end
        end
        ::continue::
    end

    error("Should return from loop")
end

function main()
    local file = io.open("input.txt", "r")
    if not file then
        error("Could not open input.txt")
    end
    local input_str = file:read("*a")
    file:close()

    -- Trim trailing newline if present
    input_str = input_str:match("^%s*(.-)%s*$")

    local result = amphipod(input_str)
    print(result)
end

main()
