local function read_file(filename)
  local file = io.open(filename, "r")
  if not file then
    error("Error opening file: " .. filename)
  end
  local content = file:read("*a")
  file:close()
  return content
end

local function parse_input(input)
  local ans = {}
  for line in input:gmatch("[^\n]+") do
    local pair = {}
    for node in line:gmatch("[^-]+") do
      table.insert(pair, node)
    end
    table.insert(ans, pair)
  end
  return ans
end

local function walk(graph, current, visited, path, double_used)
  if current == "end" then
    return 1
  end

  visited[current] = (visited[current] or 0) + 1

  local paths_to_end = 0

  for visitable, _ in pairs(graph[current]) do
    if visitable == "start" then
      goto continue
    end

    local visited_count = visited[visitable] or 0
    if visitable:match("^[a-z]+$") and visited_count > 0 then
      if double_used then
        goto continue
      else
        double_used = true
      end
    end

    table.insert(path, visitable)
    paths_to_end = paths_to_end + walk(graph, visitable, visited, path, double_used)
    table.remove(path)

    if visitable:match("^[a-z]+$") and visited_count == 1 then
      double_used = false
    end

    ::continue::
  end

  visited[current] = visited[current] - 1
  return paths_to_end
end

local function solve(input)
  local parsed = parse_input(input)

  local graph = {}
  for _, pair in ipairs(parsed) do
    if not graph[pair[1]] then
      graph[pair[1]] = {}
    end
    if not graph[pair[2]] then
      graph[pair[2]] = {}
    end
    graph[pair[1]][pair[2]] = true
    graph[pair[2]][pair[1]] = true
  end

  return walk(graph, "start", {}, {"start"}, false)
end

local input = read_file("input.txt")
print(solve(input))