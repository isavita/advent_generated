
local Room = {}
Room.__index = Room

function Room.new(name)
  local self = setmetatable({}, Room)
  self.name = name
  self.connections = {} -- map<string, Room or nil>
  return self
end

local Mode = {
  EXPLORE = 0,
  NAVIGATE = 1,
  TEST = 2,
}

local opposite = {
  north = "south",
  south = "north",
  west = "east",
  east = "west",
}

local EmulatorStatus = {
  HALTED = 0,
  OUTPUT = 1,
  WAITING_FOR_INPUT = 2,
}

local Emulator = {}
Emulator.__index = Emulator

function Emulator.new(program, input_values)
  local self = setmetatable({}, Emulator)
  self.memory = {}
  for i, v in ipairs(program) do
    self.memory[i - 1] = v -- 0-based indexing for memory addresses
  end
  self.input = {}
  if input_values then
    for _, v in ipairs(input_values) do
      table.insert(self.input, v)
    end
  end
  self.ip = 0
  self.relative_base = 0
  return self
end

function Emulator:write_string(s)
  for i = 1, #s do
    table.insert(self.input, string.byte(s, i))
  end
  return #s, nil
end

function Emulator:_get_mem(addr)
    return self.memory[addr] or 0
end

function Emulator:_set_mem(addr, value)
    if addr < 0 then error("Negative memory address") end
    self.memory[addr] = value
end


function Emulator:_get_parameter(offset)
  local instruction = self:_get_mem(self.ip)
  local mode = math.floor(instruction / (10 ^ (offset + 1))) % 10
  local param = self:_get_mem(self.ip + offset)

  if mode == 0 then -- Position mode
    return self:_get_mem(param)
  elseif mode == 1 then -- Immediate mode
    return param
  elseif mode == 2 then -- Relative mode
    return self:_get_mem(self.relative_base + param)
  else
    error("Unknown parameter mode: " .. mode)
  end
end

function Emulator:_get_write_address(offset)
  local instruction = self:_get_mem(self.ip)
  local mode = math.floor(instruction / (10 ^ (offset + 1))) % 10
  local param = self:_get_mem(self.ip + offset)
  local address

  if mode == 0 then -- Position mode
    address = param
  elseif mode == 2 then -- Relative mode
    address = self.relative_base + param
  else
    error("Invalid mode for writing: " .. mode)
  end
  return address
end

function Emulator:emulate()
  while true do
    local instruction = self:_get_mem(self.ip)
    local opcode = instruction % 100

    if opcode == 1 then -- add
      local a = self:_get_parameter(1)
      local b = self:_get_parameter(2)
      local c = self:_get_write_address(3)
      self:_set_mem(c, a + b)
      self.ip = self.ip + 4
    elseif opcode == 2 then -- multiply
      local a = self:_get_parameter(1)
      local b = self:_get_parameter(2)
      local c = self:_get_write_address(3)
      self:_set_mem(c, a * b)
      self.ip = self.ip + 4
    elseif opcode == 3 then -- input
      if #self.input == 0 then
        return nil, EmulatorStatus.WAITING_FOR_INPUT
      end
      local a = self:_get_write_address(1)
      local val = table.remove(self.input, 1)
      self:_set_mem(a, val)
      self.ip = self.ip + 2
    elseif opcode == 4 then -- output
      local a = self:_get_parameter(1)
      self.ip = self.ip + 2
      return a, EmulatorStatus.OUTPUT
    elseif opcode == 5 then -- jump-if-true
      local a = self:_get_parameter(1)
      local b = self:_get_parameter(2)
      if a ~= 0 then
        self.ip = b
      else
        self.ip = self.ip + 3
      end
    elseif opcode == 6 then -- jump-if-false
      local a = self:_get_parameter(1)
      local b = self:_get_parameter(2)
      if a == 0 then
        self.ip = b
      else
        self.ip = self.ip + 3
      end
    elseif opcode == 7 then -- less than
      local a = self:_get_parameter(1)
      local b = self:_get_parameter(2)
      local c = self:_get_write_address(3)
      self:_set_mem(c, a < b and 1 or 0)
      self.ip = self.ip + 4
    elseif opcode == 8 then -- equals
      local a = self:_get_parameter(1)
      local b = self:_get_parameter(2)
      local c = self:_get_write_address(3)
      self:_set_mem(c, a == b and 1 or 0)
      self.ip = self.ip + 4
    elseif opcode == 9 then -- relative base offset
      local a = self:_get_parameter(1)
      self.relative_base = self.relative_base + a
      self.ip = self.ip + 2
    elseif opcode == 99 then -- halt
      return nil, EmulatorStatus.HALTED
    else
      error("Unknown opcode: " .. opcode .. " at position " .. self.ip)
    end
  end
end

local function read_file(filename)
  local file = io.open(filename, "r")
  if not file then return nil end
  local content = file:read("*a")
  file:close()
  -- Simple trim for potential leading/trailing whitespace
  return content:match("^%s*(.-)%s*$")
end

local function split_string(s, sep)
    local result = {}
    for part in string.gmatch(s, "([^" .. sep .. "]+)") do
        table.insert(result, part)
    end
    return result
end


local function find_path(from_room, to_room)
  local queue = {{from_room, {from_room}}}
  local visited = {[from_room.name] = true}

  while #queue > 0 do
    local current_pair = table.remove(queue, 1)
    local current = current_pair[1]
    local path = current_pair[2]

    if current == to_room then
      return path
    end

    for _, neighbor in pairs(current.connections) do
      if neighbor and not visited[neighbor.name] then
        visited[neighbor.name] = true
        local new_path = {}
        for _, node in ipairs(path) do table.insert(new_path, node) end
        table.insert(new_path, neighbor)
        table.insert(queue, {neighbor, new_path})
      end
    end
  end
  return nil
end

function main()
  local text = read_file("input.txt")
  if not text then
    print("Error reading input.txt")
    return
  end

  local program_str = split_string(text, ",")
  local program = {}
  for _, s in ipairs(program_str) do
    table.insert(program, tonumber(s))
  end

  local emulator = Emulator.new(program)

  local function send_command(format_str, ...)
    local cmd = string.format(format_str, ...)
    emulator:write_string(cmd)
  end

  local world = {} -- map<string, Room>
  local inventory = {} -- map<string, boolean>
  local mode = Mode.EXPLORE
  local path = {} -- list<Room>
  local checkpoint = nil -- Room
  local floor = nil -- Room
  local test_dir = ""
  local available_items = {} -- list<string>
  local item_mask = 0
  local last = nil -- Room
  local last_items = {} -- list<string>
  local last_dir = ""
  local output_builder = {} -- list<int>
  local current_room = nil -- Room

  while true do
    local char, status = emulator:emulate()

    if status == EmulatorStatus.HALTED then
      local output = ""
      for _, c in ipairs(output_builder) do output = output .. string.char(c) end
      local result = output:match("typing (%d+) on the keypad")
      if result then
         print(result)
      else
         -- print("Final output:\n", output) -- For debugging
      end
      return

    elseif status == EmulatorStatus.OUTPUT then
      if char then
        table.insert(output_builder, char)
      end

    elseif status == EmulatorStatus.WAITING_FOR_INPUT then
      local output = ""
      for _, c in ipairs(output_builder) do output = output .. string.char(c) end
      output_builder = {}

      local items = {}
      local current_line_idx = 1
      local lines = {}
      for line in output:gmatch("([^\n]+)") do table.insert(lines, line) end


      local function trim(s) return s:match("^%s*(.-)%s*$") end

      local parse_state = "start" -- start, doors, items_here

      for i = 1, #lines do
         local line = trim(lines[i])

         if line == "" or line == "Command?" then
            parse_state = "start"
            goto continue_line_loop
         end

         local room_match = line:match("^== (.-) ==$")
         if room_match then
             local name = room_match
             if not world[name] then
                 current_room = Room.new(name)
                 world[name] = current_room
             else
                 current_room = world[name]
             end
             items = {}
             parse_state = "start"
             goto continue_line_loop
         end

         if line == "Doors here lead:" then
            parse_state = "doors"
            goto continue_line_loop
         end

         if line == "Items here:" then
            parse_state = "items_here"
            goto continue_line_loop
         end

         local list_match = line:match("^%- (.-)$")
         if list_match then
             if parse_state == "doors" and current_room then
                 local direction = list_match
                 if current_room.connections[direction] == nil then -- Use nil to represent unknown connection explicitly
                    current_room.connections[direction] = false -- Use false for not yet explored
                 end
             elseif parse_state == "items_here" then
                 local item = list_match
                 table.insert(items, item)
             end
             goto continue_line_loop
         end


         local taken_match = line:match("^You take the (.-)%.$")
         if taken_match then
           local taken = taken_match
           inventory[taken] = true
           if last then
               current_room = last
               local new_last_items = {}
               for _, item in ipairs(last_items) do
                   if item ~= taken then table.insert(new_last_items, item) end
               end
               items = new_last_items -- Update local 'items' reflecting the change
           end
           parse_state = "start"
           goto continue_line_loop
         end

         local dropped_match = line:match("^You drop the (.-)%.$")
         if dropped_match then
           local dropped = dropped_match
           inventory[dropped] = false
            if last then
                current_room = last
                local new_last_items = {}
                for _, item in ipairs(last_items) do table.insert(new_last_items, item) end
                table.insert(new_last_items, dropped)
                items = new_last_items -- Update local 'items'
            end
            parse_state = "start"
           goto continue_line_loop
         end

         if line:find('A loud, robotic voice says "Alert!') then
           if mode == Mode.EXPLORE then
             if #path > 0 then table.remove(path) end -- Backtrack logical path
             checkpoint, floor, test_dir = last, current_room, last_dir
             if checkpoint and test_dir and checkpoint.connections[test_dir] == false then -- Mark pressure plate connection
                 checkpoint.connections[test_dir] = floor
             end
             -- Don't connect floor back automatically, it might kill
           end
           last, last_items, last_dir = nil, {}, "" -- Reset last state as we were ejected
           parse_state = "start"
           goto continue_line_loop -- Skip rest of alert message
         end

         -- Ignore other lines for now
         ::continue_line_loop::
      end

      -- Update connections based on successful movement
      if last and last_dir ~= "" and current_room then
          if last.connections[last_dir] == false then -- If unexplored
              last.connections[last_dir] = current_room
              -- Only add reverse connection if not the pressure plate room
              if current_room ~= floor then
                 current_room.connections[opposite[last_dir]] = last
              end
          end
      end

      last, last_items, last_dir = current_room, {}, "" -- Reset for next command, clear last_dir until a move command is sent
       -- Copy items found in the room description to last_items *after* potential take/drop updates
      for _, item in ipairs(items) do table.insert(last_items, item) end


      if mode == Mode.EXPLORE then
        local blacklist = {
            ["photons"] = true, ["escape pod"] = true, ["molten lava"] = true,
            ["infinite loop"] = true, ["giant electromagnet"] = true
        }
        local took_item = false
        for _, item in ipairs(items) do
          if not blacklist[item] then
            send_command("take %s\n", item)
            took_item = true
            break
          end
        end

        if not took_item then
          local target = nil
          for direction, room in pairs(current_room.connections) do
            if room == false then -- Unexplored path found
              table.insert(path, current_room)
              target = direction
              break
            end
          end

          if target then
            last_dir = target -- Mark the direction we intend to go
            send_command("%s\n", target)
            goto continue_loop
          end

          if #path > 0 then
            local last_room = table.remove(path) -- Pop from path to backtrack
            local back_dir = nil
            for direction, room in pairs(current_room.connections) do
              if room == last_room then
                back_dir = direction
                break
              end
            end
            if back_dir then
              last_dir = back_dir
              send_command("%s\n", back_dir)
              goto continue_loop
            else
              error('Cannot go from "' .. current_room.name .. '" to "' .. last_room.name .. '"')
            end
          end

          -- Explored everywhere, found checkpoint?
          if checkpoint and floor then
            local nav_path = find_path(current_room, checkpoint)
            if nav_path then
                path = {} -- Clear old exploration path
                -- Start path from the *next* room after current
                for i = 2, #nav_path do table.insert(path, nav_path[i]) end
            else
                 error("Cannot find path back to checkpoint from " .. current_room.name)
            end
            mode = Mode.NAVIGATE
            goto continue_loop -- Continue to NAVIGATE mode logic immediately
          end
        end

      elseif mode == Mode.NAVIGATE then
        if #path > 0 then
          local next_room = table.remove(path, 1) -- Get next step
          local direction = nil
          for dir, room in pairs(current_room.connections) do
            if room == next_room then
              direction = dir
              break
            end
          end
          if direction then
            last_dir = direction
            send_command("%s\n", direction)
            goto continue_loop
          else
             error('Cannot navigate from "' .. current_room.name .. '" to "' .. next_room.name .. '"')
          end
        else -- Arrived at checkpoint
          available_items = {}
          for item, has in pairs(inventory) do
            if has then table.insert(available_items, item) end
          end
          table.sort(available_items) -- Ensure consistent order for masking
          item_mask = 0 -- Start trying combinations (start with 0 = drop all held items)
          mode = Mode.TEST
          -- Fall through to TEST mode logic
        end
      end -- Mode EXPLORE/NAVIGATE handled, now TEST check

      if mode == Mode.TEST then
        local needs_action = false
        for index, item in ipairs(available_items) do
          local target_state = (item_mask & (1 << (index - 1))) ~= 0
          if inventory[item] ~= target_state then
            local action = target_state and "take" or "drop"
            send_command("%s %s\n", action, item)
            needs_action = true
            break -- Only one action per input cycle
          end
        end

        if not needs_action then
          -- Inventory matches the current mask, try the pressure plate
          item_mask = item_mask + 1
          if item_mask >= (1 << #available_items) then
             -- This shouldn't happen if solution exists
             error("Tried all item combinations!")
          end

          if test_dir ~= "" then
             last_dir = test_dir -- Set expectation for next room (floor)
             send_command("%s\n", test_dir)
             goto continue_loop
          else
             error("Test direction (test_dir) is not set.")
          end
        end
        -- If needs_action was true, loop will continue waiting for output of take/drop
      end

       ::continue_loop::
    end -- End WAITING_FOR_INPUT
  end -- End while true
end

main()
