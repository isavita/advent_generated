
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

  local dp = {}
  for i = 1, nr do
    dp[i] = {}
    for j = 1, nc do
      dp[i][j] = -1
    end
  end

  local dirs = {{1,0},{-1,0},{0,1},{0,-1}}

  local function dfs(r, c)
    if dp[r][c] ~= -1 then
      return dp[r][c]
    end
    local h = grid[r][c]
    if h == 9 then
      dp[r][c] = 1
      return 1
    end
    local sum = 0
    for _, d in ipairs(dirs) do
      local nr2, nc2 = r + d[1], c + d[2]
      if nr2 < 1 or nr2 > nr or nc2 < 1 or nc2 > nc then
        goto continue
      end
      if grid[nr2][nc2] == h + 1 then
        sum = sum + dfs(nr2, nc2)
      end
      ::continue::
    end
    dp[r][c] = sum
    return sum
  end

  local total = 0
  for r = 1, nr do
    for c = 1, nc do
      if grid[r][c] == 0 then
        total = total + dfs(r, c)
      end
    end
  end
  print(total)
end

solve()
