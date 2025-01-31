
local function solve()
    local file = io.open("input.txt", "r")
    local content = file:read("*a")
    file:close()

    local graph = {}
    for line in string.gmatch(content, "[^\n]+") do
        if line ~= "" then
            local row = {}
            for char in string.gmatch(line, ".") do
                table.insert(row, char)
            end
            table.insert(graph, row)
        end
    end

    local H = #graph
    local W = #graph[1]

    local move = {
        {label = "left", x = -1, y = 0},
        {label = "up", x = 0, y = -1},
        {label = "right", x = 1, y = 0},
        {label = "down", x = 0, y = 1}
    }

    local sum = 0

    local function saveOuter(label, side, x, y)
        local key
        if label == "up" or label == "down" then
            key = y .. ":" .. x
        else
            key = x .. ":" .. y
        end

        if not side[label] then
            side[label] = {}
        end
        side[label][key] = true
    end

    local function check(ary, i, j)
      local search = {
          i .. ":" .. (j - 1),
          i .. ":" .. (j + 1)
      }
      for _, s in ipairs(search) do
          for _, a in ipairs(ary) do
              if a == s then
                  return true
              end
          end
      end
      return false
  end

    local function countOuter(side)
        local outer = 0
        for label, _ in pairs(side) do
            local array = {}
            for key, _ in pairs(side[label]) do
                table.insert(array, key)
            end

            table.sort(array, function(a, b)
                local aParts = {}
                for part in string.gmatch(a, "[^:]+") do
                    table.insert(aParts, tonumber(part))
                end
                local bParts = {}
                for part in string.gmatch(b, "[^:]+") do
                    table.insert(bParts, tonumber(part))
                end
                if aParts[1] == bParts[1] then
                    return aParts[2] < bParts[2]
                else
                    return aParts[1] < bParts[1]
                end
            end)

            local temp = {}
            for _, current in ipairs(array) do
              local parts = {}
                for part in string.gmatch(current, "[^:]+") do
                    table.insert(parts, tonumber(part))
                end
                if not check(temp, parts[1], parts[2]) then
                    outer = outer + 1
                end
                table.insert(temp, current)
            end
        end
        return outer
    end

    for y = 1, H do
        for x = 1, W do
            if graph[y][x] ~= "." then
                local area = 0
                local target = graph[y][x]
                local visited = {}
                local side = {}

                local function search(cx, cy, label)
                    if graph[cy][cx] ~= target then
                        if label ~= "" and not visited[cx .. "," .. cy] then
                            saveOuter(label, side, cx, cy)
                        end
                        return
                    end

                    visited[cx .. "," .. cy] = true
                    area = area + 1
                    graph[cy][cx] = "."

                    for _, m in ipairs(move) do
                        local nx = cx + m.x
                        local ny = cy + m.y

                        if nx < 1 or nx > W or ny < 1 or ny > H then
                            saveOuter(m.label, side, nx, ny)
                        else
                          if not visited[nx .. "," .. ny] then
                            search(nx, ny, m.label)
                          end
                        end
                    end
                end

                search(x, y, "")
                local outer = countOuter(side)
                sum = sum + area * outer
            end
        end
    end

    print(sum)
end

solve()
