
function parseClaim(s)
    local c = {}
    c.id, c.left, c.top, c.width, c.height = s:match("#(%d+) @ (%d+),(%d+): (%d+)x(%d+)")
    return c
end

function readClaims(filename)
    local claims = {}
    for line in io.lines(filename) do
        local claim = parseClaim(line)
        table.insert(claims, claim)
    end
    return claims
end

function countOverlappingInches(claims)
    local fabric = {}
    for _, claim in ipairs(claims) do
        for i = claim.left, claim.left + claim.width - 1 do
            for j = claim.top, claim.top + claim.height - 1 do
                local coord = i .. "," .. j
                fabric[coord] = (fabric[coord] or 0) + 1
            end
        end
    end

    local overlapping = 0
    for _, count in pairs(fabric) do
        if count > 1 then
            overlapping = overlapping + 1
        end
    end
    return overlapping
end

local claims = readClaims("input.txt")
local overlapping = countOverlappingInches(claims)
print(overlapping)
