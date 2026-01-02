
-- minimal‑presses solver ----------------------------------------------------
local function popcount(x)
    local c = 0
    while x > 0 do
        x = x & (x - 1)
        c = c + 1
    end
    return c
end

local function gaussianMinWeight(mat, R, C)   -- mat[r][1..C+1], last column = RHS
    local colIsPivot = {}
    local pivotRow = 1

    for c = 1, C do
        local sel = nil
        for r = pivotRow, R do
            if mat[r][c] == 1 then sel = r; break end
        end
        if not sel then goto continue end

        -- swap rows
        mat[pivotRow], mat[sel] = mat[sel], mat[pivotRow]

        -- eliminate other rows
        for r = 1, R do
            if r ~= pivotRow and mat[r][c] == 1 then
                for k = c, C + 1 do
                    mat[r][k] = mat[r][k] ~ mat[pivotRow][k]
                end
            end
        end
        colIsPivot[c] = true
        pivotRow = pivotRow + 1
        ::continue::
    end

    -- inconsistent rows?
    for r = pivotRow, R do
        if mat[r][C + 1] == 1 then return -1 end
    end

    -- free variables
    local freeVars = {}
    for c = 1, C do
        if not colIsPivot[c] then freeVars[#freeVars + 1] = c end
    end
    local nFree = #freeVars
    local limit = 1 << nFree
    local best = math.huge

    for mask = 0, limit - 1 do
        local x = {}
        for i = 1, C do x[i] = 0 end
        local weight = popcount(mask)

        for j = 1, nFree do
            if (mask >> (j - 1)) & 1 == 1 then
                x[freeVars[j]] = 1
            end
        end

        local prow = 1
        for c = 1, C do
            if colIsPivot[c] then
                local val = mat[prow][C + 1]
                for k = c + 1, C do
                    if mat[prow][k] == 1 then val = val ~ x[k] end
                end
                x[c] = val
                if val == 1 then weight = weight + 1 end
                prow = prow + 1
            end
        end

        if weight < best then best = weight end
    end
    return best
end
-- -------------------------------------------------------------------------

local totalPresses = 0
local f = io.open("input.txt", "r")
if not f then io.stderr:write("cannot open input.txt\n"); os.exit(1) end

for line in f:lines() do
    local l = line:gsub("\r", "")               -- strip possible CR
    local b1, b2 = l:find("%[")
    if not b1 then goto nextline end
    local e1, e2 = l:find("%]", b2)
    if not e1 then goto nextline end

    local targetStr = l:sub(b2 + 1, e1 - 1)
    local R = #targetStr
    local target = {}
    for i = 1, R do
        target[i] = (targetStr:sub(i, i) == "#") and 1 or 0
    end

    -- parse button lists
    local buttons = {}
    local pos = e1 + 1
    while true do
        local o = l:find("%(", pos)
        if not o then break end
        local c = l:find("%)", o)
        if not c then break end
        local inside = l:sub(o + 1, c - 1)
        local btn = {}
        for num in inside:gmatch("([^,]+)") do
            btn[#btn + 1] = tonumber(num)
        end
        buttons[#buttons + 1] = btn
        pos = c + 1
    end
    local C = #buttons

    -- build matrix (R rows, C+1 columns)
    local mat = {}
    for r = 1, R do
        mat[r] = {}
        for c = 1, C do
            local val = 0
            for _, idx in ipairs(buttons[c]) do
                if idx + 1 == r then val = 1; break end   -- input indices are 0‑based
            end
            mat[r][c] = val
        end
        mat[r][C + 1] = target[r]
    end

    local mw = gaussianMinWeight(mat, R, C)
    if mw ~= -1 then totalPresses = totalPresses + mw end

    ::nextline::
end

f:close()
print(totalPresses)
