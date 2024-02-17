
function countOrbits(orbitMap, start, depth)
    orbits = get(orbitMap, start, [])
    if isempty(orbits)
        return depth
    end
    count = depth
    for orbit in orbits
        count += countOrbits(orbitMap, orbit, depth+1)
    end
    return count
end

data = read("input.txt", String)
lines = split(strip(data), '\n')
orbitMap = Dict{String, Vector{String}}()
for line in lines
    parts = split(line, ')')
    center, orbiter = parts[1], parts[2]
    push!(get!(orbitMap, center, []), orbiter)
end

totalOrbits = countOrbits(orbitMap, "COM", 0)
println(totalOrbits)
