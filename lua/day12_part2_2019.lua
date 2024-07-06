-- Read input from file
local function read_input()
    local file = io.open("input.txt", "r")
    if not file then error("Could not open input file") end
    local moons = {}
    for line in file:lines() do
        local x, y, z = line:match("<x=(%-?%d+), y=(%-?%d+), z=(%-?%d+)>")
        table.insert(moons, {pos = {x = tonumber(x), y = tonumber(y), z = tonumber(z)}, vel = {x = 0, y = 0, z = 0}})
    end
    file:close()
    return moons
end

-- Apply gravity for a single axis
local function apply_gravity_axis(moons, axis)
    for i = 1, #moons do
        for j = i + 1, #moons do
            local diff = moons[j].pos[axis] - moons[i].pos[axis]
            if diff ~= 0 then
                local sign = diff > 0 and 1 or -1
                moons[i].vel[axis] = moons[i].vel[axis] + sign
                moons[j].vel[axis] = moons[j].vel[axis] - sign
            end
        end
    end
end

-- Apply velocity for a single axis
local function apply_velocity_axis(moons, axis)
    for _, moon in ipairs(moons) do
        moon.pos[axis] = moon.pos[axis] + moon.vel[axis]
    end
end

-- Find period for a single axis
local function find_period_axis(initial_moons, axis)
    local moons = {}
    for i, moon in ipairs(initial_moons) do
        moons[i] = {pos = {[axis] = moon.pos[axis]}, vel = {[axis] = 0}}
    end
    
    local states = {}
    local steps = 0
    
    while true do
        local state = ""
        for _, moon in ipairs(moons) do
            state = state .. moon.pos[axis] .. "," .. moon.vel[axis] .. ","
        end
        
        if states[state] then
            return steps
        end
        
        states[state] = true
        steps = steps + 1
        
        apply_gravity_axis(moons, axis)
        apply_velocity_axis(moons, axis)
    end
end

-- Calculate Greatest Common Divisor (GCD)
local function gcd(a, b)
    while b ~= 0 do
        a, b = b, a % b
    end
    return a
end

-- Calculate Least Common Multiple (LCM)
local function lcm(a, b)
    return (a * b) / gcd(a, b)
end

-- Main function
local function main()
    local moons = read_input()
    
    local period_x = find_period_axis(moons, "x")
    local period_y = find_period_axis(moons, "y")
    local period_z = find_period_axis(moons, "z")
    
    local total_period = lcm(lcm(period_x, period_y), period_z)
    
    print("Number of steps to reach the first previous state: " .. total_period)
end

main()
