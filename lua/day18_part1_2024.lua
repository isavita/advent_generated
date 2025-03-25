
local grid_size = 71
local corrupted = {}

local function solve()
  local file = io.open("input.txt", "r")
  if not file then return end

  local i = 0
  for line in file:lines() do
    i = i + 1
    if i > 1024 then break end
    local x, y = line:match("(%d+),(%d+)")
    x = tonumber(x)
    y = tonumber(y)
    corrupted[x * grid_size + y] = true
  end
  file:close()

  local q = { {0, 0, 0} }
  local visited = { [0] = true }
  local head, tail = 1, 1

  while head <= tail do
    local x, y, steps = table.unpack(q[head])
    head = head + 1

    if x == grid_size - 1 and y == grid_size - 1 then
      print(steps)
      return
    end

    for _, d in ipairs({{0, 1}, {0, -1}, {1, 0}, {-1, 0}}) do
      local dx, dy = d[1], d[2]
      local nx, ny = x + dx, y + dy
      if nx >= 0 and nx < grid_size and ny >= 0 and ny < grid_size then
        local index = nx * grid_size + ny
        if not corrupted[index] and not visited[index] then
          q[tail + 1] = {nx, ny, steps + 1}
          tail = tail + 1
          visited[index] = true
        end
      end
    end
  end
end

local start_time = os.clock()
solve()
local end_time = os.clock()
local elapsed_time = end_time - start_time
print(string.format("Elapsed time: %.4f seconds", elapsed_time))
