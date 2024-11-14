
using Printf

mutable struct Valve
    id::String
    flow::Int
    tunnels::Dict{String, Int}
end

function floyd_warshall!(valves::Dict{String, Valve})
    for k in keys(valves)
        for i in keys(valves)
            for j in keys(valves)
                if haskey(valves[i].tunnels, k) && haskey(valves[k].tunnels, j)
                    dik = valves[i].tunnels[k]
                    dkj = valves[k].tunnels[j]
                    dij = get(valves[i].tunnels, j, typemax(Int))
                    
                    if dik + dkj < dij
                        valves[i].tunnels[j] = dik + dkj
                    end
                end
            end
        end
    end
end

function max_pressure(valves::Dict{String, Valve}, curr::String, minute::Int, pressure::Int, 
                      open::Vector{String}, depth::Int)::Int
    max_p = pressure
    
    for next in open
        new_open = filter(x -> x != next, open)
        time_left = minute - valves[curr].tunnels[next] - 1
        
        if time_left > 0
            new_pressure = pressure + time_left * valves[next].flow
            max_p = max(max_p, max_pressure(valves, next, time_left, new_pressure, new_open, depth+1))
        end
    end
    
    return max_p
end

function main()
    # Read input
    input = strip(read("input.txt", String))
    lines = split(input, "\n")
    
    # Parse valves
    valves = Dict{String, Valve}()
    
    for line in lines
        parts = split(line, "; ")
        id_flow = split(parts[1], " has flow rate=")
        id = id_flow[1][7:end]
        flow = parse(Int, id_flow[2])
        
        tunnel_part = replace(parts[2], r"tunnel(s)? lead(s)? to valve(s)? " => "")
        tunnels = split(tunnel_part, ", ")
        
        valve = Valve(id, flow, Dict{String, Int}(id => 0))
        for t in tunnels
            valve.tunnels[t] = 1
        end
        
        valves[id] = valve
    end
    
    # Floyd-Warshall to compute shortest paths
    floyd_warshall!(valves)
    
    # Find valves with non-zero flow
    open_valves = [v.id for (_, v) in valves if v.flow > 0]
    
    # Solve
    result = max_pressure(valves, "AA", 30, 0, open_valves, 0)
    println(result)
end

main()
