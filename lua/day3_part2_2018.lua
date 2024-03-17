-- Read input from "input.txt"
local input = {}
for line in io.lines("input.txt") do
    table.insert(input, line)
end

-- Parse the input
local claims = {}
for _, line in ipairs(input) do
    local id, x, y, w, h = line:match("#(%d+) @ (%d+),(%d+): (%d+)x(%d+)")
    claims[tonumber(id)] = {
        x = tonumber(x),
        y = tonumber(y),
        w = tonumber(w),
        h = tonumber(h)
    }
end

-- Part 1: Find the number of square inches of fabric within two or more claims
local fabric = {}
for id, claim in pairs(claims) do
    for i = claim.x, claim.x + claim.w - 1 do
        for j = claim.y, claim.y + claim.h - 1 do
            fabric[i .. "," .. j] = (fabric[i .. "," .. j] or 0) + 1
        end
    end
end

local overlapped = 0
for _, count in pairs(fabric) do
    if count > 1 then
        overlapped = overlapped + 1
    end
end

print("Part 1 answer:", overlapped)

-- Part 2: Find the ID of the only claim that doesn't overlap
for id, claim in pairs(claims) do
    local noOverlap = true
    for i = claim.x, claim.x + claim.w - 1 do
        for j = claim.y, claim.y + claim.h - 1 do
            if (fabric[i .. "," .. j] or 0) > 1 then
                noOverlap = false
                break
            end
        end
        if not noOverlap then
            break
        end
    end
    if noOverlap then
        print("Part 2 answer:", id)
        break
    end
end