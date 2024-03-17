-- Read input from file
local file = io.open("input.txt", "r")
local lines = {}
for line in file:lines() do
    table.insert(lines, line)
end
file:close()

-- Create Cave struct
local Cave = {}
function Cave:new()
    local obj = {
        connections = {}
    }
    setmetatable(obj, self)
    self.__index = self
    return obj
end

function Cave:connect_to(name)
    self.connections[name] = true
end

function Cave:disconnect_from(name)
    self.connections[name] = nil
end

-- Build the cave system
local caves = {}
for _, line in ipairs(lines) do
    local paths = {}
    for path in line:gmatch("([^-]+)") do
        table.insert(paths, path)
    end
    local from, to = paths[1], paths[2]
    if not caves[from] then
        caves[from] = Cave:new()
    end
    if not caves[to] then
        caves[to] = Cave:new()
    end
    caves[from]:connect_to(to)
    caves[to]:connect_to(from)
end

-- Depth-first search
local count = 0
local function dfs(current, visited)
    if current == "end" then
        count = count + 1
        return
    end

    for next in pairs(caves[current].connections) do
        if visited[next] and next:lower() == next then
            goto continue
        end

        local visited_copy = {}
        for k, v in pairs(visited) do
            visited_copy[k] = v
        end
        visited_copy[next] = true
        dfs(next, visited_copy)
        ::continue::
    end
end

dfs("start", { start = true })
print(count)