using DelimitedFiles

struct Point
    x::Int
    y::Int
    z::Int
    t::Int
end

abs(x::Int) = x < 0 ? -x : x

function manhattan_distance(a::Point, b::Point)
    return abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z) + abs(a.t - b.t)
end

mutable struct UnionFind
    parent::Vector{Int}
end

function UnionFind(size::Int)
    parent = collect(1:size)
    return UnionFind(parent)
end

function find(uf::UnionFind, x::Int)
    if uf.parent[x] != x
        uf.parent[x] = find(uf, uf.parent[x])
    end
    return uf.parent[x]
end

function union!(uf::UnionFind, x::Int, y::Int)
    root_x = find(uf, x)
    root_y = find(uf, y)
    if root_x != root_y
        uf.parent[root_x] = root_y
    end
end

points = Point[]
open("input.txt", "r") do file
    for line in eachline(file)
        coords = parse.(Int, split(line, ","))
        push!(points, Point(coords[1], coords[2], coords[3], coords[4]))
    end
end

uf = UnionFind(length(points))
for i in 1:length(points)
    for j in 1:length(points)
        if manhattan_distance(points[i], points[j]) <= 3
            union!(uf, i, j)
        end
    end
end

global constellation_count = 0
for i in 1:length(uf.parent)
    if uf.parent[i] == i
        global constellation_count += 1
    end
end

println(constellation_count)