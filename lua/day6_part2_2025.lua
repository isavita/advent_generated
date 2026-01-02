
local function readLines()
    local f = assert(io.open("input.txt", "r"))
    local t = {}
    for line in f:lines() do
        t[#t + 1] = line
    end
    f:close()
    return t
end

local function maxWidth(lines)
    local m = 0
    for i = 1, #lines do
        local l = #lines[i]
        if l > m then m = l end
    end
    return m
end

local function isSeparator(lines, col, rows)
    for r = 1, rows do
        local ch = lines[r]:sub(col, col)
        if ch ~= "" and not ch:match("%s") then
            return false
        end
    end
    return true
end

local function processBlock(lines, startCol, endCol, rows)
    local nums = {}
    local op = '*'
    for c = startCol, endCol do
        local sb = {}
        local hasDigits = false
        for r = 1, rows do
            local line = lines[r]
            if c <= #line then
                local ch = line:sub(c, c)
                if ch:match("%d") then
                    sb[#sb + 1] = ch
                    hasDigits = true
                elseif ch == '+' then
                    op = '+'
                elseif ch == '*' then
                    op = '*'
                end
            end
        end
        if hasDigits then
            nums[#nums + 1] = tonumber(table.concat(sb))
        end
    end

    if #nums == 0 then return 0 end

    if op == '*' then
        local prod = 1
        for i = 1, #nums do prod = prod * nums[i] end
        return prod
    else
        local sum = 0
        for i = 1, #nums do sum = sum + nums[i] end
        return sum
    end
end

local function main()
    local lines = readLines()
    if #lines == 0 then
        print("Grand total: 0")
        return
    end

    local maxW = maxWidth(lines)
    local rows = #lines
    local sep = {}
    for x = 1, maxW do
        sep[x] = isSeparator(lines, x, rows)
    end

    local total = 0
    local inBlock = false
    local startCol = 1

    for x = 1, maxW do
        if not sep[x] then
            if not inBlock then
                inBlock = true
                startCol = x
            end
        else
            if inBlock then
                total = total + processBlock(lines, startCol, x - 1, rows)
                inBlock = false
            end
        end
    end

    if inBlock then
        total = total + processBlock(lines, startCol, maxW, rows)
    end

    print("Grand total: " .. total)
end

main()
