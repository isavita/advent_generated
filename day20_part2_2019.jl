const Wall = '#'
const Free = '.'

struct P
    X::UInt16
    Y::UInt16
end

Neighbours(p::P) = [P(p.X, p.Y + 1), P(p.X + 1, p.Y), P(p.X, p.Y - 1), P(p.X - 1, p.Y)]

mutable struct Map
    XMax::UInt16
    YMax::UInt16
    Grid::Dict{P,Char}
    AA::P
    ZZ::P
    Teleport::Dict{P,P}
    PortalName::Dict{P,String}
    IsOuter::Dict{P,Bool}
end

function parse()
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

        pName, pPoint, ok = extractPortal(grid, P(i, j))

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

function extractPortal(grid, p)
    c1 = get(grid, p, ' ')

    if haskey(grid, P(p.X + 1, p.Y))
        c2 = grid[P(p.X + 1, p.Y)]
        if isletter(c2)
            portalName = string(c1, c2)

            portalPoint = P(p.X + 2, p.Y)
            if get(grid, portalPoint, ' ') == '.'
                return portalName, portalPoint, true
            end

            portalPoint = P(p.X - 1, p.Y)
            if get(grid, portalPoint, ' ') == '.'
                return portalName, portalPoint, true
            end
        end
    end

    if haskey(grid, P(p.X, p.Y + 1))
        c2 = grid[P(p.X, p.Y + 1)]
        if isletter(c2)
            portalName = string(c1, c2)

            portalPoint = P(p.X, p.Y + 2)
            if get(grid, portalPoint, ' ') == '.'
                return portalName, portalPoint, true
            end

            portalPoint = P(p.X, p.Y - 1)
            if get(grid, portalPoint, ' ') == '.'
                return portalName, portalPoint, true
            end
        end
    end

    return "", P(0, 0), false
end

struct Status
    P::P
    Depth::Int
end

function BFSNested(m::Map)
    discovered = Set{Status}()
    toDo = Status[]

    root = Status(m.AA, 0)

    push!(discovered, root)
    push!(toDo, root)

    steps = 0

    while !isempty(toDo)
        for levelSize in 1:length(toDo)
            curr = popfirst!(toDo)

            for n in Neighbours(curr.P)
                dest = get(m.Grid, n, ' ')

                if dest == Wall
                    continue
                elseif dest == Free
                    target = Status(n, curr.Depth)

                    if !(target in discovered)
                        push!(discovered, target)
                        push!(toDo, target)
                    end
                elseif isletter(dest)
                    isOuter = get(m.IsOuter, curr.P, false)

                    if !isOuter
                        target = Status(m.Teleport[curr.P], curr.Depth + 1)
                    else
                        portalName = m.PortalName[curr.P]
                        if curr.Depth == 0
                            if portalName == "ZZ"
                                return steps
                            end
                            continue
                        end

                        if portalName == "AA" || portalName == "ZZ"
                            continue
                        end

                        target = Status(m.Teleport[curr.P], curr.Depth - 1)
                    end

                    if !(target in discovered)
                        push!(discovered, target)
                        push!(toDo, target)
                    end
                end
            end
        end

        steps += 1
    end

    return -1
end

m = parse()
println(BFSNested(m))