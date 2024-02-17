
struct Particle
    p::Array{Int, 1}
    v::Array{Int, 1}
    a::Array{Int, 1}
end

particles = Particle[]

open("input.txt") do file
    for line in eachline(file)
        parts = split(line, ", ")
        p = Particle(zeros(Int, 3), zeros(Int, 3), zeros(Int, 3))
        for (i, part) in enumerate(parts)
            coords = split(part[4:end-1], ",")
            for (j, coord) in enumerate(coords)
                num = parse(Int, coord)
                if i == 1
                    p.p[j] = num
                elseif i == 2
                    p.v[j] = num
                elseif i == 3
                    p.a[j] = num
                end
            end
        end
        push!(particles, p)
    end
end

for tick in 1:1000
    positions = Dict{String, Int}()
    for (i, particle) in enumerate(particles)
        for j in 1:3
            particle.v[j] += particle.a[j]
            particle.p[j] += particle.v[j]
        end
        particles[i] = particle
        posStr = string(particle.p[1], ",", particle.p[2], ",", particle.p[3])
        positions[posStr] = get(positions, posStr, 0) + 1
    end

    newParticles = Particle[]
    for particle in particles
        posStr = string(particle.p[1], ",", particle.p[2], ",", particle.p[3])
        if positions[posStr] == 1
            push!(newParticles, particle)
        end
    end
    global particles = newParticles
end

println(length(particles))
