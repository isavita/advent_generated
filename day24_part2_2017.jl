
struct Component
    a::Int
    b::Int
end

global maxStrength = 0
global maxLength = 0

function findStrongestLongestBridge(components, used, port, strength, length)
    global maxStrength
    global maxLength
    
    if length > maxLength || (length == maxLength && strength > maxStrength)
        maxStrength = strength
        maxLength = length
    end

    for (i, c) in enumerate(components)
        if used[i]
            continue
        end

        if c.a == port || c.b == port
            used[i] = true
            nextPort = c.a
            if c.a == port
                nextPort = c.b
            end
            findStrongestLongestBridge(components, used, nextPort, strength+c.a+c.b, length+1)
            used[i] = false
        end
    end
end

components = []

open("input.txt") do file
    for line in eachline(file)
        ports = split(line, "/")
        a = parse(Int, ports[1])
        b = parse(Int, ports[2])
        push!(components, Component(a, b))
    end
end

used = falses(length(components))

findStrongestLongestBridge(components, used, 0, 0, 0)

println(maxStrength)
