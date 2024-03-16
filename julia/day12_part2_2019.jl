mutable struct Vec3
    x::Int
    y::Int
    z::Int
end

mutable struct Moon
    pos::Vec3
    vel::Vec3
end

function apply_gravity(moons, axis)
    for i in 1:length(moons)
        for j in i+1:length(moons)
            if axis == "x"
                if moons[i].pos.x > moons[j].pos.x
                    moons[i].vel.x -= 1
                    moons[j].vel.x += 1
                elseif moons[i].pos.x < moons[j].pos.x
                    moons[i].vel.x += 1
                    moons[j].vel.x -= 1
                end
            elseif axis == "y"
                if moons[i].pos.y > moons[j].pos.y
                    moons[i].vel.y -= 1
                    moons[j].vel.y += 1
                elseif moons[i].pos.y < moons[j].pos.y
                    moons[i].vel.y += 1
                    moons[j].vel.y -= 1
                end
            elseif axis == "z"
                if moons[i].pos.z > moons[j].pos.z
                    moons[i].vel.z -= 1
                    moons[j].vel.z += 1
                elseif moons[i].pos.z < moons[j].pos.z
                    moons[i].vel.z += 1
                    moons[j].vel.z -= 1
                end
            end
        end
    end
end

function apply_velocity(moons, axis)
    for i in 1:length(moons)
        if axis == "x"
            moons[i].pos.x += moons[i].vel.x
        elseif axis == "y"
            moons[i].pos.y += moons[i].vel.y
        elseif axis == "z"
            moons[i].pos.z += moons[i].vel.z
        end
    end
end

function find_cycle(moons, initial_moons, axis)
    steps = 1
    while true
        apply_gravity(moons, axis)
        apply_velocity(moons, axis)

        match = true
        for i in 1:length(moons)
            if axis == "x"
                if moons[i].pos.x != initial_moons[i].pos.x || moons[i].vel.x != initial_moons[i].vel.x
                    match = false
                    break
                end
            elseif axis == "y"
                if moons[i].pos.y != initial_moons[i].pos.y || moons[i].vel.y != initial_moons[i].vel.y
                    match = false
                    break
                end
            elseif axis == "z"
                if moons[i].pos.z != initial_moons[i].pos.z || moons[i].vel.z != initial_moons[i].vel.z
                    match = false
                    break
                end
            end
        end

        if match
            return steps
        end

        steps += 1
    end
end

function gcd(a, b)
    while b != 0
        a, b = b, a % b
    end
    return a
end

function lcm(a, b)
    return div(a * b, gcd(a, b))
end

moons = []
initial_moons = []

open("input.txt", "r") do file
    for line in eachline(file)
        x, y, z = parse.(Int, match(r"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>", line).captures)
        push!(moons, Moon(Vec3(x, y, z), Vec3(0, 0, 0)))
        push!(initial_moons, Moon(Vec3(x, y, z), Vec3(0, 0, 0)))
    end
end

cycle_x = find_cycle(moons, initial_moons, "x")
cycle_y = find_cycle(moons, initial_moons, "y")
cycle_z = find_cycle(moons, initial_moons, "z")

lcm_xy = lcm(cycle_x, cycle_y)
lcm_xyz = lcm(lcm_xy, cycle_z)

println(lcm_xyz)