
function readAndParseInput(filename)
    distances = Dict{String, Dict{String, Int}}()
    file = open(filename, "r")
    for line in eachline(file)
        parts = split(line, " ")
        if length(parts) != 5
            continue
        end
        from, to, dist = parts[1], parts[3], parse(Int, parts[5])
        if !haskey(distances, from)
            distances[from] = Dict{String, Int}()
        end
        distances[from][to] = dist
        if !haskey(distances, to)
            distances[to] = Dict{String, Int}()
        end
        distances[to][from] = dist
    end
    close(file)
    return distances
end

function getUniqueLocations(distances)
    locationSet = Set{String}()
    for (from, _) in distances
        push!(locationSet, from)
        for (to, _) in distances[from]
            push!(locationSet, to)
        end
    end
    return collect(locationSet)
end

function findLongestRoute(locations, distances)
    maxDistance = Ref(0)
    permute(locations, 1, maxDistance, distances, false)
    return maxDistance[]
end

function permute(arr, i, bestDistance, distances, findShortest)
    if i > length(arr)
        return
    end
    if i == length(arr)
        dist = calculateRouteDistance(arr, distances)
        if findShortest
            if bestDistance[] == 0 || dist < bestDistance[]
                bestDistance[] = dist
            end
        else
            if dist > bestDistance[]
                bestDistance[] = dist
            end
        end
        return
    end
    for j = i:length(arr)
        arr[i], arr[j] = arr[j], arr[i]
        permute(arr, i+1, bestDistance, distances, findShortest)
        arr[i], arr[j] = arr[j], arr[i]
    end
end

function calculateRouteDistance(route, distances)
    sum = 0
    for i in 1:length(route)-1
        sum += distances[route[i]][route[i+1]]
    end
    return sum
end

distances = readAndParseInput("input.txt")
locations = getUniqueLocations(distances)
maxDistance = findLongestRoute(locations, distances)
println(maxDistance)
