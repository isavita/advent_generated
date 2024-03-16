using Printf

mutable struct Vec3
    x::Int
    y::Int
    z::Int
end

mutable struct Moon
    pos::Vec3
    vel::Vec3
end

function abs(x::Int)
    return x < 0 ? -x : x
end

function apply_gravity!(moons::Vector{Moon})
    for i in 1:length(moons)
        for j in i+1:length(moons)
            if moons[i].pos.x > moons[j].pos.x
                moons[i].vel.x -= 1
                moons[j].vel.x += 1
            elseif moons[i].pos.x < moons[j].pos.x
                moons[i].vel.x += 1
                moons[j].vel.x -= 1
            end

            if moons[i].pos.y > moons[j].pos.y
                moons[i].vel.y -= 1
                moons[j].vel.y += 1
            elseif moons[i].pos.y < moons[j].pos.y
                moons[i].vel.y += 1
                moons[j].vel.y -= 1
            end

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

function apply_velocity!(moons::Vector{Moon})
    for m in moons
        m.pos.x += m.vel.x
        m.pos.y += m.vel.y
        m.pos.z += m.vel.z
    end
end

function total_energy(moons::Vector{Moon})
    total = 0
    for m in moons
        pot = abs(m.pos.x) + abs(m.pos.y) + abs(m.pos.z)
        kin = abs(m.vel.x) + abs(m.vel.y) + abs(m.vel.z)
        total += pot * kin
    end
    return total
end

function main()
    moons = Vector{Moon}()
    open("input.txt", "r") do file
        for line in eachline(file)
            x, y, z = parse.(Int, match(r"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>", line).captures)
            push!(moons, Moon(Vec3(x, y, z), Vec3(0, 0, 0)))
        end
    end

    for step in 1:1000
        apply_gravity!(moons)
        apply_velocity!(moons)
    end

    @printf("%d\n", total_energy(moons))
end

main()