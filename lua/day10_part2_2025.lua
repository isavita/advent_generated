
local function main()
    local file = io.open("input.txt", "r")
    if not file then error("cannot open input.txt") end
    local content = file:read("*a")
    file:close()

    local total = 0
    for line in content:gmatch("[^\r\n]+") do
        local btns = {}
        for part in line:gmatch("%((.-)%)") do
            local t = {}
            for v in part:gmatch("%d+") do t[#t+1] = tonumber(v) end
            btns[#btns+1] = t
        end
        local targets = {}
        for v in line:match("%{(.-)%}"):gmatch("%d+") do
            targets[#targets+1] = tonumber(v)
        end
        total = total + solve(btns, targets)
    end
    print(total)
end

function solve(buttons, targets)
    local n, m = #targets, #buttons
    local mat = {}
    for i = 1, n do
        mat[i] = {}
        for j = 1, m+1 do mat[i][j] = 0 end
        mat[i][m+1] = targets[i]
    end
    for i = 1, m do
        for _, j in ipairs(buttons[i]) do
            if j < n then mat[j+1][i] = 1 end
        end
    end

    local pivCol = {}
    for i = 1, n do pivCol[i] = -1 end
    local row = 1
    for col = 1, m do
        if row > n then break end
        local maxRow = row
        for r = row+1, n do
            if math.abs(mat[r][col]) > math.abs(mat[maxRow][col]) then maxRow = r end
        end
        if math.abs(mat[maxRow][col]) < 1e-9 then goto continue end
        mat[row], mat[maxRow] = mat[maxRow], mat[row]
        local scale = mat[row][col]
        for c = col, m+1 do mat[row][c] = mat[row][c] / scale end
        for r = 1, n do
            if r ~= row and math.abs(mat[r][col]) > 1e-9 then
                local f = mat[r][col]
                for c = col, m+1 do mat[r][c] = mat[r][c] - f * mat[row][c] end
            end
        end
        pivCol[row] = col
        row = row + 1
        ::continue::
    end
    local rank = row - 1
    local isPivot, pivRow = {}, {}
    for i = 1, m do isPivot[i] = false; pivRow[i] = -1 end
    for r = 1, rank do
        local c = pivCol[r]
        if c > 0 then isPivot[c] = true; pivRow[c] = r end
    end
    local freeVars = {}
    for i = 1, m do if not isPivot[i] then freeVars[#freeVars+1] = i end end

    local maxPresses = {}
    for i = 1, m do
        local limit = math.maxinteger
        for _, j in ipairs(buttons[i]) do
            if j < n and targets[j+1] < limit then limit = targets[j+1] end
        end
        if limit == math.maxinteger then limit = 0 end
        maxPresses[i] = limit
    end
    table.sort(freeVars, function(a,b) return maxPresses[a] < maxPresses[b] end)

    local best = math.maxinteger
    local freeVals = {}

    local function computePivots()
        local res = {}
        for i = 1, m do res[i] = 0 end
        for i = 1, #freeVars do res[freeVars[i]] = freeVals[i] end
        for r = rank, 1, -1 do
            local c = pivCol[r]
            if c <= 0 then goto nextp end
            local v = mat[r][m+1]
            for j = c+1, m do v = v - mat[r][j] * res[j] end
            local iv = math.floor(v + 0.5)
            if math.abs(v - iv) > 1e-6 or iv < 0 or iv > maxPresses[c] then return nil end
            res[c] = iv
            ::nextp::
        end
        return res
    end

    local function enumerate(idx, cur)
        if cur >= best then return end
        if idx > #freeVars then
            local arr = computePivots()
            if arr then
                local sum = 0
                for i = 1, m do sum = sum + arr[i] end
                if sum < best then best = sum end
            end
            return
        end
        local fv = freeVars[idx]
        local limit = maxPresses[fv]
        for v = 0, limit do
            freeVals[idx] = v
            enumerate(idx + 1, cur + v)
        end
    end

    enumerate(1, 0)
    return best == math.maxinteger and -1 or best
end

main()
