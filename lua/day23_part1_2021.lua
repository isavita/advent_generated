
--[[
--- Day 23: Amphipod ---

Solution using Dijkstra's algorithm on the state space graph.
Each state represents the configuration of the burrow.
Edges represent valid moves with associated energy costs.
We seek the shortest path (minimum energy) from the initial state to the goal state.
]]

-- Utility function for deep copying a table (array part)
local function copy_table(orig)
    local copy = {}
    for i = 1, #orig do
        copy[i] = orig[i]
    end
    return copy
end

-- Min-Heap implementation for Priority Queue
local MinHeap = {}
MinHeap.__index = MinHeap

function MinHeap:create()
    -- Store nodes as {cost = number, state_key = string, state_table = table}
    local heap = {nodes = {}, count = 0, lookup = {}} -- lookup maps state_key to index in nodes
    setmetatable(heap, MinHeap)
    return heap
end

function MinHeap:_swap(i, j)
    local nodes = self.nodes
    local lookup = self.lookup
    lookup[nodes[i].state_key] = j
    lookup[nodes[j].state_key] = i
    nodes[i], nodes[j] = nodes[j], nodes[i]
end

function MinHeap:_bubble_up(index)
    local nodes = self.nodes
    while index > 1 do
        local parent_index = math.floor(index / 2)
        if nodes[index].cost < nodes[parent_index].cost then
            self:_swap(index, parent_index)
            index = parent_index
        else
            break
        end
    end
    self.lookup[nodes[index].state_key] = index
end

function MinHeap:_bubble_down(index)
    local nodes = self.nodes
    local count = self.count
    local lookup = self.lookup
    while true do
        local left_child_index = index * 2
        local right_child_index = index * 2 + 1
        local smallest_index = index

        if left_child_index <= count and nodes[left_child_index].cost < nodes[smallest_index].cost then
            smallest_index = left_child_index
        end
        if right_child_index <= count and nodes[right_child_index].cost < nodes[smallest_index].cost then
            smallest_index = right_child_index
        end

        if smallest_index ~= index then
            self:_swap(index, smallest_index)
            index = smallest_index
        else
            break
        end
    end
    if nodes[index] then -- Ensure the node still exists after potential swaps
       lookup[nodes[index].state_key] = index
    end
end

function MinHeap:push(cost, state_key, state_table)
    local existing_index = self.lookup[state_key]
    if existing_index then
        -- Update existing node if new cost is lower
        if cost < self.nodes[existing_index].cost then
            self.nodes[existing_index].cost = cost
            self:_bubble_up(existing_index)
        end
    else
        -- Add new node
        local node = {cost = cost, state_key = state_key, state_table = state_table}
        self.count = self.count + 1
        self.nodes[self.count] = node
        self.lookup[state_key] = self.count
        self:_bubble_up(self.count)
    end
end

function MinHeap:pop()
    if self.count == 0 then return nil end
    local nodes = self.nodes
    local root = nodes[1]
    self.lookup[root.state_key] = nil -- Remove from lookup

    if self.count > 1 then
        nodes[1] = nodes[self.count]
        self.lookup[nodes[1].state_key] = 1
    end

    nodes[self.count] = nil -- Remove reference
    self.count = self.count - 1

    if self.count > 0 then
        self:_bubble_down(1)
    end
    return root
end

function MinHeap:is_empty()
    return self.count == 0
end

-- Constants and Configuration
local HALLWAY_SIZE = 11
local ROOM_DEPTH = 2
local NUM_ROOMS = 4

-- 1-based indexing for Lua tables
-- Hallway: indices 1 to 11
-- Rooms:
--   Room A (0): 12 (top), 13 (bottom)
--   Room B (1): 14 (top), 15 (bottom)
--   Room C (2): 16 (top), 17 (bottom)
--   Room D (3): 18 (top), 19 (bottom)
local TOTAL_SPOTS = HALLWAY_SIZE + NUM_ROOMS * ROOM_DEPTH -- 11 + 4*2 = 19

local EMPTY = '.'
local ENERGY_COST = { A = 1, B = 10, C = 100, D = 1000 }
local DEST_ROOM_TYPE = { 'A', 'B', 'C', 'D' } -- Type for room index 0, 1, 2, 3
local TYPE_DEST_ROOM_IDX = { A = 0, B = 1, C = 2, D = 3 } -- Room index for a given type

-- Map room index (0-3) to entrance hallway index (1-based)
local ROOM_ENTRANCE_HALLWAY_IDX = { 3, 5, 7, 9 } -- Corresponds to rooms A, B, C, D

-- Hallway indices where stopping is forbidden
local FORBIDDEN_HALLWAY_IDX = { [3] = true, [5] = true, [7] = true, [9] = true }

-- Map linear index (1-based) to room index (0-3) or nil
local function get_room_idx(idx)
    if idx >= 12 and idx <= 13 then return 0 end -- Room A
    if idx >= 14 and idx <= 15 then return 1 end -- Room B
    if idx >= 16 and idx <= 17 then return 2 end -- Room C
    if idx >= 18 and idx <= 19 then return 3 end -- Room D
    return nil
end

-- Map linear index (1-based) to room depth (1=top, 2=bottom) or nil
local function get_room_depth(idx)
    local room_idx = get_room_idx(idx)
    if room_idx == nil then return nil end
    -- Top spots: 12, 14, 16, 18. Bottom spots: 13, 15, 17, 19
    return (idx % 2 == 0) and 1 or 2 -- If even index -> top (depth 1), else bottom (depth 2)
end

-- Get the linear indices for a given room index (0-3)
local function get_room_indices(room_idx)
    local top_idx = 12 + room_idx * 2
    local bottom_idx = top_idx + 1
    return top_idx, bottom_idx
end

-- Check if a hallway path is clear between two hallway indices (exclusive)
local function is_hallway_path_clear(state_table, start_idx, end_idx)
    local dir = (start_idx < end_idx) and 1 or -1
    for i = start_idx + dir, end_idx - dir, dir do
        if state_table[i] ~= EMPTY then
            return false
        end
    end
    return true
end

-- Check if path is clear to exit a room from a specific spot
local function is_room_exit_path_clear(state_table, start_idx)
    local depth = get_room_depth(start_idx)
    if depth == 2 then -- Bottom spot
        local top_idx = start_idx - 1
        return state_table[top_idx] == EMPTY
    elseif depth == 1 then -- Top spot
        return true -- Always clear to move from top spot to hallway entrance if top is occupied
    end
    return false -- Should not happen if start_idx is valid room spot
end

-- Check if a room is ready for an amphipod of `target_type` to enter
-- Room must be empty or contain only amphipods of the correct type
local function is_room_valid_for_entry(state_table, room_idx, target_type)
    local top_idx, bottom_idx = get_room_indices(room_idx)
    local correct_type = DEST_ROOM_TYPE[room_idx + 1]
    if target_type ~= correct_type then return false end -- Sanity check

    local occupant_bottom = state_table[bottom_idx]
    local occupant_top = state_table[top_idx]

    if occupant_bottom ~= EMPTY and occupant_bottom ~= correct_type then
        return false -- Incorrect type at bottom
    end
    if occupant_top ~= EMPTY and occupant_top ~= correct_type then
        return false -- Incorrect type at top
    end

    return true -- Room is valid (empty or contains only correct types)
end

-- Find the deepest available spot in a valid destination room
local function get_target_room_spot(state_table, room_idx)
    local top_idx, bottom_idx = get_room_indices(room_idx)
    if state_table[bottom_idx] == EMPTY then
        return bottom_idx, 2 -- Return index and depth (steps from entrance)
    elseif state_table[top_idx] == EMPTY then
        return top_idx, 1
    else
        return nil -- Room is full or something is wrong
    end
end

-- Check if an amphipod at `idx` is already in its final correct position
local function is_in_final_position(state_table, idx, type)
    local room_idx = get_room_idx(idx)
    if room_idx == nil then return false end -- Not in a room

    local correct_type = DEST_ROOM_TYPE[room_idx + 1]
    if type ~= correct_type then return false end -- In the wrong room type

    local depth = get_room_depth(idx)
    if depth == 1 then -- Top spot
        -- Must also check the bottom spot
        local _, bottom_idx = get_room_indices(room_idx)
        return state_table[bottom_idx] == correct_type -- Final only if bottom is also correct
    elseif depth == 2 then -- Bottom spot
        -- Already in the deepest spot of the correct room
        return true
    end
    return false -- Should not happen
end

-- Generate all possible next states from a given state
local function get_next_states(state_table)
    local next_states = {} -- { {cost = number, next_state_table = table}, ... }

    for idx = 1, TOTAL_SPOTS do
        local type = state_table[idx]
        if type ~= EMPTY then
            local energy_mult = ENERGY_COST[type]
            local current_room_idx = get_room_idx(idx)

            -- Case 1: Amphipod is in the Hallway (1-11)
            if current_room_idx == nil then
                local dest_room_idx = TYPE_DEST_ROOM_IDX[type]
                local entrance_idx = ROOM_ENTRANCE_HALLWAY_IDX[dest_room_idx + 1]

                -- Check if destination room is valid for entry
                if is_room_valid_for_entry(state_table, dest_room_idx, type) then
                    -- Check if hallway path to entrance is clear
                    if is_hallway_path_clear(state_table, idx, entrance_idx) then
                        local target_room_spot, steps_into_room = get_target_room_spot(state_table, dest_room_idx)
                        if target_room_spot then
                            local steps_hallway = math.abs(idx - entrance_idx)
                            local total_steps = steps_hallway + steps_into_room
                            local cost = total_steps * energy_mult

                            local next_state_table = copy_table(state_table)
                            next_state_table[target_room_spot] = type
                            next_state_table[idx] = EMPTY
                            table.insert(next_states, { cost = cost, next_state_table = next_state_table })
                        end
                    end
                end
            -- Case 2: Amphipod is in a Room (12-19)
            else
                -- Optimization: Don't move if already in final position
                if not is_in_final_position(state_table, idx, type) then
                    -- Check if path out of the room is clear
                    if is_room_exit_path_clear(state_table, idx) then
                        local entrance_idx = ROOM_ENTRANCE_HALLWAY_IDX[current_room_idx + 1]
                        local steps_to_exit = get_room_depth(idx) -- 1 if top, 2 if bottom

                        -- Try moving to each valid hallway spot
                        for hall_target_idx = 1, HALLWAY_SIZE do
                            if not FORBIDDEN_HALLWAY_IDX[hall_target_idx] then -- Can stop here
                                -- Check if hallway path from entrance to target is clear
                                if is_hallway_path_clear(state_table, entrance_idx, hall_target_idx) then
                                    local steps_hallway = math.abs(entrance_idx - hall_target_idx)
                                    local total_steps = steps_to_exit + steps_hallway
                                    local cost = total_steps * energy_mult

                                    local next_state_table = copy_table(state_table)
                                    next_state_table[hall_target_idx] = type
                                    next_state_table[idx] = EMPTY
                                    table.insert(next_states, { cost = cost, next_state_table = next_state_table })
                                end
                            end
                        end
                    end
                end
            end
        end
    end
    return next_states
end

-- Convert state table to a string key
local function state_to_key(state_table)
    return table.concat(state_table)
end

-- Main function to solve the puzzle
function main()
    local file = io.open("input.txt", "r")
    if not file then
        print("Error: Could not open input.txt")
        return
    end
    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()

    -- Parse initial state
    -- #############    Line 1
    -- #...........#    Line 2: Hallway indices 1-11
    -- ###B#C#B#D###    Line 3: Room top spots 12, 14, 16, 18
    --   #A#D#C#A#      Line 4: Room bottom spots 13, 15, 17, 19
    --   #########      Line 5
    local initial_state_table = {}
    for i = 1, TOTAL_SPOTS do initial_state_table[i] = EMPTY end -- Initialize

    -- Hallway (always empty initially)
    -- Handled by initialization

    -- Room top spots (Line 3)
    initial_state_table[12] = string.sub(lines[3], 4, 4) -- Room A Top
    initial_state_table[14] = string.sub(lines[3], 6, 6) -- Room B Top
    initial_state_table[16] = string.sub(lines[3], 8, 8) -- Room C Top
    initial_state_table[18] = string.sub(lines[3], 10, 10) -- Room D Top

    -- Room bottom spots (Line 4)
    initial_state_table[13] = string.sub(lines[4], 4, 4) -- Room A Bottom
    initial_state_table[15] = string.sub(lines[4], 6, 6) -- Room B Bottom
    initial_state_table[17] = string.sub(lines[4], 8, 8) -- Room C Bottom
    initial_state_table[19] = string.sub(lines[4], 10, 10) -- Room D Bottom

    local initial_state_key = state_to_key(initial_state_table)

    -- Define Goal State
    local goal_state_table = {}
    for i = 1, HALLWAY_SIZE do goal_state_table[i] = EMPTY end
    goal_state_table[12], goal_state_table[13] = 'A', 'A'
    goal_state_table[14], goal_state_table[15] = 'B', 'B'
    goal_state_table[16], goal_state_table[17] = 'C', 'C'
    goal_state_table[18], goal_state_table[19] = 'D', 'D'
    local goal_state_key = state_to_key(goal_state_table)

    -- Dijkstra's Algorithm
    local dist = {} -- Maps state_key to minimum cost
    local pq = MinHeap:create()

    dist[initial_state_key] = 0
    pq:push(0, initial_state_key, initial_state_table)

    local min_energy = -1

    while not pq:is_empty() do
        local current_node = pq:pop()
        local u_key = current_node.state_key
        local u_cost = current_node.cost
        local u_table = current_node.state_table

        -- If cost in PQ is higher than already found shortest path, skip
        if u_cost > dist[u_key] then
            goto continue -- Lua's equivalent of continue
        end

        -- Goal check
        if u_key == goal_state_key then
            min_energy = u_cost
            break
        end

        -- Explore neighbors
        local next_possible_states = get_next_states(u_table)
        for _, move in ipairs(next_possible_states) do
            local v_table = move.next_state_table
            local move_cost = move.cost
            local v_key = state_to_key(v_table)
            local new_cost = u_cost + move_cost

            if dist[v_key] == nil or new_cost < dist[v_key] then
                dist[v_key] = new_cost
                pq:push(new_cost, v_key, v_table)
            end
        end
        ::continue:: -- Label for goto
    end

    -- Print the result
    if min_energy ~= -1 then
        print(min_energy)
    else
        print("Solution not found.")
    end

end

-- Run the main function
main()
