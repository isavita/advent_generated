
local function trim(s)
    return (s:gsub("^%s+", ""):gsub("%s+$", ""))
end

local function normalize(p)
    local minr, minc = math.huge, math.huge
    for _,pt in ipairs(p) do
        if pt.r < minr then minr = pt.r end
        if pt.c < minc then minc = pt.c end
    end
    local res = {}
    for _,pt in ipairs(p) do
        table.insert(res, {r = pt.r - minr, c = pt.c - minc})
    end
    table.sort(res, function(a, b)
        return a.r == b.r and a.c < b.c or a.r < b.r
    end)
    return res
end

local function rotate(p)
    local r = {}
    for _,pt in ipairs(p) do
        table.insert(r, {r = pt.c, c = -pt.r})
    end
    return r
end

local function flip(p)
    local r = {}
    for _,pt in ipairs(p) do
        table.insert(r, {r = pt.r, c = -pt.c})
    end
    return r
end

local function key(p)
    local s = {}
    for _,pt in ipairs(p) do
        table.insert(s, pt.r .. "," .. pt.c)
    end
    return table.concat(s, ";")
end

local function generateVariations(base)
    local uniq = {}
    local set = {}
    local cur = base
    for i = 1, 4 do
        local n = normalize(cur)
        local k = key(n)
        if not set[k] then set[k] = true; table.insert(uniq, n) end
        local f = flip(cur)
        local nf = normalize(f)
        local kf = key(nf)
        if not set[kf] then set[kf] = true; table.insert(uniq, nf) end
        cur = rotate(cur)
    end
    return uniq
end

local function canPlace(rows, cols, grid, p, rr, cc)
    for _,pt in ipairs(p) do
        local nr, nc = rr + pt.r, cc + pt.c
        if nr < 0 or nr >= rows or nc < 0 or nc >= cols then return false end
        if grid[nr * cols + nc] ~= 0 then return false end
    end
    return true
end

local function place(cols, grid, p, rr, cc, val)
    for _,pt in ipairs(p) do
        grid[(rr + pt.r) * cols + (cc + pt.c)] = val
    end
end

local function checkIslands(rows, cols, grid, counts, arrSize, slackIdx, shapes)
    local minReal = math.huge
    local hasReal = false
    for i = 1, arrSize do
        if i ~= slackIdx and counts[i] > 0 then
            hasReal = true
            if shapes[i].n < minReal then minReal = shapes[i].n end
        end
    end
    if not hasReal then return true end
    local slack = counts[slackIdx]
    local vis = {}
    for i = 0, rows * cols - 1 do vis[i] = false end
    local q = {}
    for i = 0, rows * cols - 1 do
        if grid[i] == 0 and not vis[i] then
            local qs, qe = 1, 1
            q[1] = i
            vis[i] = true
            local size = 0
            while qs <= qe do
                local cur = q[qs] qs = qs + 1
                size = size + 1
                local r, c = math.floor(cur / cols), cur % cols
                if r > 0 then
                    local n = (r - 1) * cols + c
                    if grid[n] == 0 and not vis[n] then vis[n] = true; qe = qe + 1; q[qe] = n end
                end
                if r < rows - 1 then
                    local n = (r + 1) * cols + c
                    if grid[n] == 0 and not vis[n] then vis[n] = true; qe = qe + 1; q[qe] = n end
                end
                if c > 0 then
                    local n = r * cols + (c - 1)
                    if grid[n] == 0 and not vis[n] then vis[n] = true; qe = qe + 1; q[qe] = n end
                end
                if c < cols - 1 then
                    local n = r * cols + (c + 1)
                    if grid[n] == 0 and not vis[n] then vis[n] = true; qe = qe + 1; q[qe] = n end
                end
            end
            if size < minReal then
                if slack >= size then slack = slack - size else return false end
            end
        end
    end
    return true
end

local function solveRec(rows, cols, grid, counts, arrSize, ids, idCount, variations, varCounts, slackIdx, shapes)
    local empty = -1
    for i = 0, rows * cols - 1 do
        if grid[i] == 0 then empty = i; break end
    end
    if empty == -1 then return true end
    if not checkIslands(rows, cols, grid, counts, arrSize, slackIdx, shapes) then return false end
    local r, c = math.floor(empty / cols), empty % cols
    for ii = 1, idCount do
        local id = ids[ii]
        if counts[id] == 0 then goto continue end
        counts[id] = counts[id] - 1
        for v = 1, varCounts[id] do
            local p = variations[id][v]
            if canPlace(rows, cols, grid, p, r, c) then
                place(cols, grid, p, r, c, 1)
                if solveRec(rows, cols, grid, counts, arrSize, ids, idCount, variations, varCounts, slackIdx, shapes) then return true end
                place(cols, grid, p, r, c, 0)
            end
        end
        counts[id] = counts[id] + 1
        ::continue::
    end
    return false
end

local file = io.open("input.txt", "r")
if not file then os.exit(1) end
local rawLines = {}
for line in file:lines() do table.insert(rawLines, line) end
file:close()

local maxId = -1
for _,ln in ipairs(rawLines) do
    local s = trim(ln)
    if #s > 0 and s:sub(-1) == ':' then
        local id = tonumber(s:sub(1, -2))
        if id and id > maxId then maxId = id end
    end
end
local arrSize = maxId + 2
local slackIdx = maxId + 1

local shapes = {}
for i = 1, arrSize do shapes[i] = {n = 0, p = {}} end

local parsingShapes = true
local currentID = nil
local curShapeLines = {}
local regionLines = {}

for _,ln in ipairs(rawLines) do
    local s = trim(ln)
    if #s == 0 then goto continue end
    if s:find("x") and s:find(":") then parsingShapes = false end
    if parsingShapes then
        if s:sub(-1) == ':' then
            if currentID then
                local pts = {}
                for r,line in ipairs(curShapeLines) do
                    for c=1,#line do
                        if line:sub(c,c) == "#" then
                            table.insert(pts, {r=r-1, c=c-1})
                        end
                    end
                end
                shapes[currentID+1] = {n = #pts, p = normalize(pts)}
                curShapeLines = {}
            end
            currentID = tonumber(s:sub(1, -2))
        else
            table.insert(curShapeLines, s)
        end
    else
        table.insert(regionLines, s)
    end
    ::continue::
end

if currentID then
    local pts = {}
    for r,line in ipairs(curShapeLines) do
        for c=1,#line do
            if line:sub(c,c) == "#" then
                table.insert(pts, {r=r-1, c=c-1})
            end
        end
    end
    shapes[currentID+1] = {n = #pts, p = normalize(pts)}
end

shapes[slackIdx+1] = {n = 1, p = {{r=0,c=0}}}

local variations = {}
local varCounts = {}
for i = 1, arrSize do
    if shapes[i].n == 0 then
        variations[i] = {}
        varCounts[i] = 0
    else
        variations[i] = generateVariations(shapes[i].p)
        varCounts[i] = #variations[i]
    end
end

local solved = 0
for _,ln in ipairs(regionLines) do
    local colon = ln:find(":")
    if not colon then goto rcont end
    local dims = trim(ln:sub(1, colon-1))
    local countsStr = trim(ln:sub(colon+1))
    local wx, h = dims:match("(%d+)%s*x%s*(%d+)")
    if not wx then goto rcont end
    wx, h = tonumber(wx), tonumber(h)
    local gridSize = wx * h
    local pieceCounts = {}
    for i = 1, arrSize do pieceCounts[i] = 0 end
    local totalArea = 0
    local idx = 0
    for tok in countsStr:gmatch("%S+") do
        idx = idx + 1
        local c = tonumber(tok)
        if c and c > 0 and idx <= arrSize-1 then
            pieceCounts[idx] = c
            totalArea = totalArea + c * shapes[idx].n
        end
    end
    if totalArea > gridSize then goto rcont end
    local slack = gridSize - totalArea
    if slack > 0 then pieceCounts[slackIdx+1] = slack end
    local ids = {}
    for i = 1, arrSize do if pieceCounts[i] > 0 then table.insert(ids, i) end end
    table.sort(ids, function(a,b) return shapes[a].n > shapes[b].n end)
    local grid = {}
    for i = 0, gridSize-1 do grid[i] = 0 end
    if solveRec(h, wx, grid, pieceCounts, arrSize, ids, #ids, variations, varCounts, slackIdx+1, shapes) then solved = solved + 1 end
    ::rcont::
end

print("Number of regions that fit all presents: " .. solved)
