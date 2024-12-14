
local function read_file(filename)
  local file = io.open(filename, "r")
  if not file then return nil end
  local content = file:read "*all"
  file:close()
  return content
end

local function solve()
  local data = read_file("input.txt")
  local lines = {}
  for line in data:gmatch("([^\n]+)") do
    table.insert(lines, line)
  end
  local nr = #lines
  local nc = #lines[1]
  local grid = {}
  for i = 1, nr do
    grid[i] = {}
    for j = 1, nc do
      grid[i][j] = tonumber(lines[i]:sub(j, j))
    end
  end

  local dirs = {{1,0},{-1,0},{0,1},{0,-1}}
  local trailheads = {}
  for r = 1, nr do
    for c = 1, nc do
      if grid[r][c] == 0 then
        table.insert(trailheads, {r,c})
      end
    end
  end

  local sumScores = 0
  for _, th in ipairs(trailheads) do
    local reached = {}
    local front = {{th[1],th[2],0}}
    local visited = {}
    while #front > 0 do
      local cur = table.remove(front)
      local r, c, h = cur[1], cur[2], cur[3]
      if h == 9 then
        reached[r..","..c] = true
        goto continue
      end
      for _, d in ipairs(dirs) do
        local nr2, nc2 = r+d[1], c+d[2]
        if nr2<1 or nr2>nr or nc2<1 or nc2>nc then goto continue_dir end
        if grid[nr2][nc2] == h+1 then
          local key = nr2..","..nc2..","..(h+1)
          if not visited[key] then
            visited[key] = true
            table.insert(front, {nr2,nc2,h+1})
          end
        end
        ::continue_dir::
      end
      ::continue::
    end
    local count = 0
    for _ in pairs(reached) do count = count + 1 end
    sumScores = sumScores + count
  end
  print(sumScores)
end

solve()
