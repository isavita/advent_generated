
function manhattan(x)
    return sum(abs.(x))
end

function main()
    particles = Particle[]
    
    open("input.txt", "r") do file
        for line in eachline(file)
            parts = split(line, ", ")
            
            p = zeros(Int, 3)
            v = zeros(Int, 3)
            a = zeros(Int, 3)
            
            for (i, part) in enumerate(parts)
                coords = parse.(Int, split(part[4:end-1], ","))
                
                if i == 1
                    p .= coords
                elseif i == 2
                    v .= coords
                else
                    a .= coords
                end
            end
            
            push!(particles, Particle(p, v, a))
        end
    end
    
    minAccel = typemax(Int)
    minVelocity = typemax(Int)
    minPosition = typemax(Int)
    closestParticle = 0
    
    for (i, particle) in enumerate(particles)
        accel = manhattan(particle.a)
        velocity = manhattan(particle.v)
        position = manhattan(particle.p)
        
        if accel < minAccel || 
           (accel == minAccel && velocity < minVelocity) ||
           (accel == minAccel && velocity == minVelocity && position < minPosition)
            minAccel = accel
            minVelocity = velocity
            minPosition = position
            closestParticle = i
        end
    end
    
    println(closestParticle - 1)  # Julia uses 1-based indexing
end

struct Particle
    p::Vector{Int}
    v::Vector{Int}
    a::Vector{Int}
end

main()
