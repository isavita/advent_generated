
local q1, q2, q3, q4 = 0, 0, 0, 0

local file = io.open("input.txt", "r")
for line in file:lines() do
    local p_str, v_str = line:match("p=([^%s]+) v=([^%s]+)")
    if p_str and v_str then
        local px_str, py_str = p_str:match("([^,]+),([^,]+)")
        local vx_str, vy_str = v_str:match("([^,]+),([^,]+)")
        local px = tonumber(px_str)
        local py = tonumber(py_str)
        local vx = tonumber(vx_str)
        local vy = tonumber(vy_str)
        local x = (px + vx * 100) % 101
        local y = (py + vy * 100) % 103
        if x ~= 50 and y ~= 51 then
            if x < 50 and y < 51 then
                q1 = q1 + 1
            elseif x > 50 and y < 51 then
                q2 = q2 + 1
            elseif x < 50 and y > 51 then
                q3 = q3 + 1
            elseif x > 50 and y > 51 then
                q4 = q4 + 1
            end
        end
    end
end
file:close()

print(q1 * q2 * q3 * q4)
