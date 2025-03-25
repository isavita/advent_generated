
local itertools = require("itertools")

local Halves = {}
function Halves:new(is_chip, material)
    local obj = {
        is_chip = is_chip,
        material = material
    }
    setmetatable(obj, { __index = self })
    return obj
end

function Halves:__tostring()
    local t_type = self.is_chip and " microchip" or " generator"
    return self.material .. t_type
end

local State = {}
function State:new(floors, elevator_level, steps)
    local obj = {
        floors = floors or { {}, {}, {}, {} },
        elevator_level = elevator_level or 0,
        steps = steps or 0
    }
    setmetatable(obj, { __index = self })
    return obj
end

function State:__tostring()
    local result = string.format("Level %d x Steps %d\n", self.elevator_level, self.steps)
    for i, floor in ipairs(self.floors) do
        local floor_str = ""
        for j, half in ipairs(floor) do
            floor_str = floor_str .. tostring(half) .. (j < #floor and ", " or "")
        end
        result = result .. string.format("  %d: %s\n", i - 1, floor_str)
    end
    return result
end

function State:hash_key()
    local gen_to_index = {}
    local chip_to_index = {}
    for fl_index, floor in ipairs(self.floors) do
        for _, half in ipairs(floor) do
            if half.is_chip then
                chip_to_index[half.material] = fl_index - 1
            else
                gen_to_index[half.material] = fl_index - 1
            end
        end
    end

    local gen_chip_pairs = {}
    for mat in pairs(gen_to_index) do
        table.insert(gen_chip_pairs, {gen_to_index[mat], chip_to_index[mat]})
    end

    table.sort(gen_chip_pairs, function(a, b)
        if a[1] ~= b[1] then
            return a[1] < b[1]
        end
        return a[2] < b[2]
    end)

    local key = tostring(self.elevator_level)
    for _, pair in ipairs(gen_chip_pairs) do
        key = key .. tostring(pair[1]) .. tostring(pair[2])
    end
    return key
end

function State:is_valid()
    for _, floor in ipairs(self.floors) do
        local gens_seen = {}
        for _, half in ipairs(floor) do
            if not half.is_chip then
                gens_seen[half.material] = true
            end
        end
        if next(gens_seen) == nil then
            goto continue
        end
        for _, half in ipairs(floor) do
            if half.is_chip and not gens_seen[half.material] then
                return false
            end
        end
        ::continue::
    end
    return true
end

function State:is_done()
    for i = 1, #self.floors - 1 do
        if #self.floors[i] > 0 then
            return false
        end
    end
    return true
end

function State:get_movable_perm_indices()
    local current_level = self.floors[self.elevator_level + 1]
    local indices = {}
    for i = 1, #current_level do
        table.insert(indices, i)
    end

    local perms = {}
    for r = 1, math.min(2, #indices) do
        local combinations = itertools.combinations(indices, r)
        for _, comb in ipairs(combinations) do
            table.insert(perms, comb)
        end
    end
    return perms
end

function State:clone()
    local new_floors = {}
    for _, floor in ipairs(self.floors) do
        table.insert(new_floors, {table.unpack(floor)})
    end
    return State:new(new_floors, self.elevator_level, self.steps)
end

function State:get_next_states()
    local future_states = {}
    local movable_perm_indices = self:get_movable_perm_indices()
    local ele_diffs = {}
    if self.elevator_level > 0 then table.insert(ele_diffs, -1) end
    if self.elevator_level < #self.floors - 1 then table.insert(ele_diffs, 1) end

    for _, ele_diff in ipairs(ele_diffs) do
        for _, perm_indices in ipairs(movable_perm_indices) do
            local cl = self:clone()
            cl.elevator_level = cl.elevator_level + ele_diff
            cl.steps = cl.steps + 1
            local old_level, new_level = self.elevator_level + 1, cl.elevator_level + 1

            local items_to_move = {}
            for _, index in ipairs(perm_indices) do
                table.insert(items_to_move, cl.floors[old_level][index])
            end

            table.sort(perm_indices, function(a, b) return a > b end)
            for _, index in ipairs(perm_indices) do
                table.remove(cl.floors[old_level], index)
            end

            for _, item in ipairs(items_to_move) do
                table.insert(cl.floors[new_level], item)
            end

            if cl:is_valid() then
                table.insert(future_states, cl)
            end
        end
    end
    return future_states
end

local function read_file(file_path)
    local file = io.open(file_path, "r")
    local content = file:read("*all")
    file:close()
    return content:gsub("^%s*", ""):gsub("%s*$", "")
end

local function rtg_hell_day(input_str, part)
    local lines = string.gmatch(input_str, "[^\r\n]+")
    local floors = {{}, {}, {}, {}}
    local line_index = 0
    for line in lines do
        line_index = line_index + 1
        local parts = {}
        for word in string.gmatch(line:gsub(",", ""):gsub("%.", ""), "%S+") do
            table.insert(parts, word)
        end
        for i, word in ipairs(parts) do
            if word == "generator" then
                local material = parts[i-1]
                table.insert(floors[line_index], Halves:new(false, material))
            elseif word == "microchip" then
                local material = parts[i-1]:gsub("-comp", "")
                table.insert(floors[line_index], Halves:new(true, material))
            end
        end
    end

    local initial_state = State:new(floors)
    if part == 2 then
        initial_state.floors[1][#initial_state.floors[1]+1] = Halves:new(false, "elerium")
        initial_state.floors[1][#initial_state.floors[1]+1] = Halves:new(true, "elerium")
        initial_state.floors[1][#initial_state.floors[1]+1] = Halves:new(false, "dilithium")
        initial_state.floors[1][#initial_state.floors[1]+1] = Halves:new(true, "dilithium")
    end

    local queue = {initial_state}
    local prev_states = {}
    local count = 0
    while #queue > 0 do
        local front = table.remove(queue, 1)
        if front:is_done() then
            return front.steps
        end
        local hash_key = front:hash_key()
        if prev_states[hash_key] then
            goto continue
        end
        prev_states[hash_key] = true
        local next_states = front:get_next_states()
        for _, next_state in ipairs(next_states) do
            table.insert(queue, next_state)
        end
        ::continue::
    end
    return -1
end

local input_str = read_file("input.txt")
print(rtg_hell_day(input_str, 2))
