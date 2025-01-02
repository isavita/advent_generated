
function solve()
    input = read("input.txt", String)
    lines = split(strip(input), "\n")
    valves = Dict{String, Dict{String, Int}}()
    flows = Dict{String, Int}()

    for line in lines
        parts = split(line, "; ")
        valve_part = split(parts[1], " ")
        id = valve_part[2]
        flow = parse(Int, split(valve_part[5], "=")[2])
        flows[id] = flow
        tunnels_part = split(parts[2], " ")
        tunnels = Dict{String, Int}()
        tunnels[id] = 0
        if length(tunnels_part) > 5
            for t in split(join(tunnels_part[5:end], " "), ", ")
                tunnels[t] = 1
            end
        else
            tunnels[tunnels_part[5]] = 1
        end
        valves[id] = tunnels
    end

    for k in keys(valves)
        for i in keys(valves)
            for j in keys(valves)
                if haskey(valves[i], k) && haskey(valves[k], j)
                    dik = valves[i][k]
                    dkj = valves[k][j]
                    if !haskey(valves[i], j) || valves[i][j] > dik + dkj
                        valves[i][j] = dik + dkj
                    end
                end
            end
        end
    end

    open_valves = [id for (id, flow) in flows if flow > 0]
    max_pressure = 0

    function max_pressure_helper(curr, minute, pressure, open, d)
        max_p = pressure
        for next in open
            new_open = filter(x -> x != next, open)
            time_left = minute - valves[curr][next] - 1
            if time_left > 0
                max_p = max(max_p, max_pressure_helper(next, time_left, time_left * flows[next] + pressure, new_open, d + 1))
            end
        end
        return max_p
    end

    function divide(l)
        if l == 1
            return [[[], [0]], [[0], []]]
        end
        d = divide(l - 1)
        r = []
        for i in eachindex(d)
            push!(r, [[d[i][1]..., l - 1], d[i][2]])
            push!(r, [d[i][1], [d[i][2]..., l - 1]])
        end
        return r
    end

    for d in divide(length(open_valves))
        if isempty(d[1]) || isempty(d[2])
            continue
        end
        mine = open_valves[d[1] .+ 1]
        elephant = open_valves[d[2] .+ 1]
        x = max_pressure_helper("AA", 26, 0, mine, 0) + max_pressure_helper("AA", 26, 0, elephant, 0)
        max_pressure = max(max_pressure, x)
    end
    println(max_pressure)
end

solve()
