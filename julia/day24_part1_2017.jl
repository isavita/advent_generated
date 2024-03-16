struct Component
    a::Int
    b::Int
end

function find_strongest_bridge(components, used, port, strength)
    global max_strength
    if strength > max_strength
        max_strength = strength
    end

    for (i, c) in enumerate(components)
        if used[i]
            continue
        end

        if c.a == port || c.b == port
            used[i] = true
            next_port = c.a
            if c.a == port
                next_port = c.b
            end
            find_strongest_bridge(components, used, next_port, strength + c.a + c.b)
            used[i] = false
        end
    end
end

function main()
    global max_strength = 0
    components = []
    for line in eachline("input.txt")
        ports = split(line, "/")
        a = parse(Int, ports[1])
        b = parse(Int, ports[2])
        push!(components, Component(a, b))
    end

    used = falses(length(components))
    find_strongest_bridge(components, used, 0, 0)

    println(max_strength)
end

main()