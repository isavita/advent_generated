function read_and_parse_input(filename::String)
    distances = Dict{String,Dict{String,Int}}()
    for line in readlines(filename)
        parts = split(line, " ")
        if length(parts) != 5
            continue # Invalid line format
        end
        from, to, dist = parts[1], parts[3], parse(Int, parts[5])
        if !haskey(distances, from)
            distances[from] = Dict{String,Int}()
        end
        distances[from][to] = dist
        if !haskey(distances, to)
            distances[to] = Dict{String,Int}()
        end
        distances[to][from] = dist # Assuming distance is symmetric
    end
    return distances
end

function get_unique_locations(distances::Dict{String,Dict{String,Int}})
    locations = Set{String}()
    for from in keys(distances)
        push!(locations, from)
        for to in keys(distances[from])
            push!(locations, to)
        end
    end
    return collect(locations)
end

function calculate_route_distance(route::Vector{String}, distances::Dict{String,Dict{String,Int}})
    sum = 0
    for i in 1:length(route)-1
        sum += distances[route[i]][route[i+1]]
    end
    return sum
end

function find_shortest_route(locations::Vector{String}, distances::Dict{String,Dict{String,Int}})
    min_distance = Ref{Int}(-1)
    permute(locations, 1, min_distance, distances)
    return min_distance[]
end

function permute(arr::Vector{String}, i::Int, min_distance::Ref{Int}, distances::Dict{String,Dict{String,Int}})
    if i > length(arr)
        return
    end
    if i == length(arr)
        dist = calculate_route_distance(arr, distances)
        if min_distance[] == -1 || dist < min_distance[]
            min_distance[] = dist
        end
        return
    end
    for j in i:length(arr)
        arr[i], arr[j] = arr[j], arr[i]
        permute(arr, i+1, min_distance, distances)
        arr[i], arr[j] = arr[j], arr[i]
    end
end

distances = read_and_parse_input("input.txt")
locations = get_unique_locations(distances)
min_distance = find_shortest_route(locations, distances)
println(min_distance)