local function calc(s)
    for d1 = 9, 0, -1 do
        local p = s:find(tostring(d1))
        if p and p < #s then
            local max2 = -1
            for i = p + 1, #s do
                local c = s:sub(i, i)
                if c:match("%d") then
                    local v = c:byte() - 48
                    if v > max2 then max2 = v end
                    if max2 == 9 then break end
                end
            end
            if max2 ~= -1 then return d1 * 10 + max2 end
        end
    end
    return 0
end

local total = 0
for line in io.lines("input.txt") do
    total = total + calc(line)
end
print(total)