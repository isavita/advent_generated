-- Read input from "input.txt" file
local file = io.open("input.txt", "r")
local lines = {}
for line in file:lines() do
    table.insert(lines, line)
end
file:close()

-- Ship struct
local Ship = {
    x = 0,
    y = 0,
    waypoint_x = 10,
    waypoint_y = 1
}

-- Process instructions
function Ship:process_instruction(action, value)
    if action == "N" then
        self.waypoint_y = self.waypoint_y + value
    elseif action == "S" then
        self.waypoint_y = self.waypoint_y - value
    elseif action == "E" then
        self.waypoint_x = self.waypoint_x + value
    elseif action == "W" then
        self.waypoint_x = self.waypoint_x - value
    elseif action == "L" then
        self:rotate_waypoint(-value)
    elseif action == "R" then
        self:rotate_waypoint(value)
    elseif action == "F" then
        self.x = self.x + self.waypoint_x * value
        self.y = self.y + self.waypoint_y * value
    end
end

-- Rotate waypoint
function Ship:rotate_waypoint(degrees)
    degrees = (degrees + 360) % 360
    if degrees == 90 or degrees == -270 then
        self.waypoint_x, self.waypoint_y = self.waypoint_y, -self.waypoint_x
    elseif degrees == 180 or degrees == -180 then
        self.waypoint_x, self.waypoint_y = -self.waypoint_x, -self.waypoint_y
    elseif degrees == 270 or degrees == -90 then
        self.waypoint_x, self.waypoint_y = -self.waypoint_y, self.waypoint_x
    end
end

-- Absolute value function
local function abs(x)
    return x < 0 and -x or x
end

-- Main program
local ship = Ship
for _, line in ipairs(lines) do
    local action = line:sub(1, 1)
    local value = tonumber(line:sub(2))
    ship:process_instruction(action, value)
end

-- Print the answer
local manhattan_distance = abs(ship.x) + abs(ship.y)
print(manhattan_distance)