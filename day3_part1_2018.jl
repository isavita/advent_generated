
using DelimitedFiles

struct Claim
    id::Int
    left::Int
    top::Int
    width::Int
    height::Int
end

function parse_claim(s::AbstractString)::Claim
    m = match(r"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)", s)
    Claim(parse(Int, m[1]), parse(Int, m[2]), parse(Int, m[3]), parse(Int, m[4]), parse(Int, m[5]))
end

function read_claims(filename::AbstractString)::Vector{Claim}
    open(filename) do file
        return [parse_claim(line) for line in eachline(file)]
    end
end

function count_overlapping_inches(claims::Vector{Claim})::Int
    fabric = Dict{String,Int}()
    for claim in claims
        for i in claim.left:(claim.left + claim.width - 1)
            for j in claim.top:(claim.top + claim.height - 1)
                coord = string(i, ",", j)
                haskey(fabric, coord) || (fabric[coord] = 0)
                fabric[coord] += 1
            end
        end
    end
    return sum(fabric[coord] > 1 for coord in keys(fabric))
end

claims = read_claims("input.txt")
overlapping = count_overlapping_inches(claims)
println(overlapping)
