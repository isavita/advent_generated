const Wall = '#'
const Free = '.'

struct P
    X::UInt16
    Y::UInt16
end

function neighbours(p::P)
    return [P(p.X, p.Y + 1), P(p.X + 1, p.Y), P(p.X, p.Y - 1), P(p.X - 1, p.Y)]
end

struct Map
    XMax::UInt16
    YMax::UInt16
    Grid::Dict{P,Char}
    AA::P
    ZZ::P
    Teleport::Dict{P,P}
    PortalName::Dict{P,String}
    IsOuter::Dict{P,Bool}
end

function parse_input()
    grid = Dict{P,Char}()
    xMax, yMax = UInt16(0), UInt16(0)

    open("input.txt") do file
        i = UInt16(0)
        for line in eachline(file)
            if length(line) > yMax
                yMax = UInt16(length(line))
            end

            for j in 1:length(line)
                grid[P(i, j - 1)] = line[j]
            end
            i += 1
        end
        xMax = i
    end

    aa, zz = P(0, 0), P(0, 0)
    isOuter = Dict{P,Bool}()
    portalName = Dict{P,String}()
    teleport = Dict{P,P}()

    cache = Dict{String,P}()

    for i in 0:xMax-1, j in 0:yMax-1
        c = get(grid, P(i, j), ' ')

        if !isletter(c)
            continue
        end

        pName, pPoint, ok = extract_portal(grid, P(i, j))

        if !ok
            continue
        end

        portalName[pPoint] = pName

        if pName == "AA"
            aa = pPoint
            isOuter[pPoint] = true
            continue
        end

        if pName == "ZZ"
            zz = pPoint
            isOuter[pPoint] = true
            continue
        end

        if haskey(cache, pName)
            target = cache[pName]
            teleport[pPoint] = target
            teleport[target] = pPoint
        else
            cache[pName] = pPoint
        end

        if j == 0 || i == 0 || i == xMax - 2 || j == yMax - 2
            isOuter[pPoint] = true
        else
            isOuter[pPoint] = false
        end
    end

    return Map(xMax, yMax, grid, aa, zz, teleport, portalName, isOuter)
end

function extract_portal(grid, p)
    c1 = get(grid, p, ' ')

    if haskey(grid, P(p.X + 1, p.Y))
        c2 = grid[P(p.X + 1, p.Y)]
        if isletter(c2)
            portalName = string(c1) * string(c2)

            portalPoint = P(p.X + 2, p.Y)
            if haskey(grid, portalPoint) && grid[portalPoint] == '.'
                return portalName, portalPoint, true
            end

            portalPoint = P(p.X - 1, p.Y)
            if haskey(grid, portalPoint) && grid[portalPoint] == '.'
                return portalName, portalPoint, true
            end
        end
    end

    if haskey(grid, P(p.X, p.Y + 1))
        c2 = grid[P(p.X, p.Y + 1)]
        if isletter(c2)
            portalName = string(c1) * string(c2)

            portalPoint = P(p.X, p.Y + 2)
            if haskey(grid, portalPoint) && grid[portalPoint] == '.'
                return portalName, portalPoint, true
            end

            portalPoint = P(p.X, p.Y - 1)
            if haskey(grid, portalPoint) && grid[portalPoint] == '.'
                return portalName, portalPoint, true
            end
        end
    end

    return "", P(0, 0), false
end

function bfs(m::Map)
    discovered = Set{P}()
    toDo = P[]

    push!(discovered, m.AA)
    push!(toDo, m.AA)

    depth = 0

    while !isempty(toDo)
        levelSize = length(toDo)
        while levelSize > 0
            curr = popfirst!(toDo)

            if curr == m.ZZ
                return depth
            end

            for n in neighbours(curr)
                if !haskey(m.Grid, n)
                    continue
                end
                dest = m.Grid[n]

                if dest == Wall
                    continue
                elseif dest == Free
                    if !(n in discovered)
                        push!(discovered, n)
                        push!(toDo, n)
                    end
                elseif isletter(dest)
                    if haskey(m.Teleport, curr)
                        next = m.Teleport[curr]
                        if !(next in discovered)
                            push!(discovered, next)
                            push!(toDo, next)
                        end
                    end
                end
            end
            levelSize -= 1
        end
        depth += 1
    end

    return -1
end

function main()
    m = parse_input()
    println(bfs(m))
end

main()