function abs(x)
    return math.abs(x)
end

function manhattan(x)
    return abs(x[1]) + abs(x[2]) + abs(x[3])
end

local particles = {}
local file = io.open("input.txt", "r")
for line in file:lines() do
    local parts = {}
    for part in line:gmatch("[pva]=<([^>]*)>") do
        table.insert(parts, part)
    end

    local p = {}
    for i, part in ipairs(parts) do
        local coords = {}
        for coord in part:gmatch("(-?%d+)") do
            table.insert(coords, tonumber(coord))
        end
        if i == 1 then
            p.p = coords
        elseif i == 2 then
            p.v = coords
        elseif i == 3 then
            p.a = coords
        end
    end
    table.insert(particles, p)
end
file:close()

local closestParticle = 0
local minAccel = math.huge
local minVelocity = math.huge
local minPosition = math.huge

for i, particle in ipairs(particles) do
    local accel = manhattan(particle.a)
    local velocity = manhattan(particle.v)
    local position = manhattan(particle.p)

    if accel < minAccel or (accel == minAccel and velocity < minVelocity) or
       (accel == minAccel and velocity == minVelocity and position < minPosition) then
        minAccel = accel
        minVelocity = velocity
        minPosition = position
        closestParticle = i - 1
    end
end

print(closestParticle)