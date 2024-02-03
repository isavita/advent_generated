
function Abs(x)
    if x < 0 then
        return -x
    end
    return x
end

function applyGravity(moons)
    for i = 1, #moons do
        for j = i + 1, #moons do
            if moons[i].pos.x > moons[j].pos.x then
                moons[i].vel.x = moons[i].vel.x - 1
                moons[j].vel.x = moons[j].vel.x + 1
            elseif moons[i].pos.x < moons[j].pos.x then
                moons[i].vel.x = moons[i].vel.x + 1
                moons[j].vel.x = moons[j].vel.x - 1
            end

            if moons[i].pos.y > moons[j].pos.y then
                moons[i].vel.y = moons[i].vel.y - 1
                moons[j].vel.y = moons[j].vel.y + 1
            elseif moons[i].pos.y < moons[j].pos.y then
                moons[i].vel.y = moons[i].vel.y + 1
                moons[j].vel.y = moons[j].vel.y - 1
            end

            if moons[i].pos.z > moons[j].pos.z then
                moons[i].vel.z = moons[i].vel.z - 1
                moons[j].vel.z = moons[j].vel.z + 1
            elseif moons[i].pos.z < moons[j].pos.z then
                moons[i].vel.z = moons[i].vel.z + 1
                moons[j].vel.z = moons[j].vel.z - 1
            end
        end
    end
end

function applyVelocity(moons)
    for i = 1, #moons do
        moons[i].pos.x = moons[i].pos.x + moons[i].vel.x
        moons[i].pos.y = moons[i].pos.y + moons[i].vel.y
        moons[i].pos.z = moons[i].pos.z + moons[i].vel.z
    end
end

function totalEnergy(moons)
    local total = 0
    for i = 1, #moons do
        local m = moons[i]
        local pot = Abs(m.pos.x) + Abs(m.pos.y) + Abs(m.pos.z)
        local kin = Abs(m.vel.x) + Abs(m.vel.y) + Abs(m.vel.z)
        total = total + pot * kin
    end
    return total
end

local moons = {}
for line in io.lines("input.txt") do
    local x, y, z = line:match("<x=(%-?%d+), y=(%-?%d+), z=(%-?%d+)")
    table.insert(moons, {pos = {x = tonumber(x), y = tonumber(y), z = tonumber(z)}, vel = {x = 0, y = 0, z = 0}})
end

for step = 1, 1000 do
    applyGravity(moons)
    applyVelocity(moons)
end

print(totalEnergy(moons))
