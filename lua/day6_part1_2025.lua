
local function trim(s) return s:match("^%s*(.-)%s*$") end

local function bignum_add(a, b)
    local i, j, carry, res = #a, #b, 0, {}
    while i > 0 or j > 0 or carry > 0 do
        local da = i > 0 and (a:byte(i) - 48) or 0
        local db = j > 0 and (b:byte(j) - 48) or 0
        local sum = da + db + carry
        carry = sum // 10
        res[#res + 1] = (sum % 10) + 48
        i, j = i - 1, j - 1
    end
    return string.char(table.unpack(res)):reverse()
end

local function bignum_mul(a, b)
    local al, bl = #a, #b
    if al == 0 or bl == 0 then return "0" end
    local prod = {}
    for i = 0, al + bl do prod[i] = 0 end
    for i = al, 1, -1 do
        local da = a:byte(i) - 48
        for j = bl, 1, -1 do
            prod[i + j] = prod[i + j] + da * (b:byte(j) - 48)
        end
    end
    for i = al + bl, 2, -1 do
        if prod[i] > 9 then
            prod[i - 1] = prod[i - 1] + prod[i] // 10
            prod[i] = prod[i] % 10
        end
    end
    local k = 1
    while k <= al + bl and prod[k] == 0 do k = k + 1 end
    if k > al + bl then return "0" end
    local res = {}
    for i = k, al + bl do res[#res + 1] = prod[i] + 48 end
    return string.char(table.unpack(res))
end

local lines, maxw = {}, 0
for line in io.lines("input.txt") do
    lines[#lines + 1] = line
    if #line > maxw then maxw = #line end
end
local nlines = #lines

local function is_sep(col)
    for i = 1, nlines do
        local l = lines[i]
        if col <= #l and l:sub(col, col):match("%S") then return false end
    end
    return true
end

local function process_block(sc, ec, grand)
    local nums, op = {}, 0
    for i = 1, nlines do
        local seg = lines[i]:sub(sc, ec)
        local t = trim(seg)
        if t ~= "" then
            if t == "+" then op = 1
            elseif t == "*" then op = 2
            else nums[#nums + 1] = t end
        end
    end
    if #nums == 0 then return grand end
    local acc = "0"
    if op == 1 then
        for _, v in ipairs(nums) do acc = bignum_add(acc, v) end
    elseif op == 2 then
        acc = "1"
        for _, v in ipairs(nums) do acc = bignum_mul(acc, v) end
    elseif #nums == 1 then
        acc = nums[1]
    end
    return bignum_add(grand, acc)
end

local grand = "0"
local inb, sc = false, 0
for x = 1, maxw do
    if not is_sep(x) then
        if not inb then inb, sc = true, x end
    else
        if inb then
            grand = process_block(sc, x - 1, grand)
            inb = false
        end
    end
end
if inb then grand = process_block(sc, maxw, grand) end

print("Grand total: " .. grand)
