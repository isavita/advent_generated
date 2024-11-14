
struct RangeMap
    srcStart::Int
    destStart::Int
    length::Int
end

function convertNumber(number::Int, ranges::Vector{RangeMap})::Int
    for r in ranges
        if r.srcStart ≤ number < r.srcStart + r.length
            return r.destStart + (number - r.srcStart)
        end
    end
    return number
end

function main()
    open("input.txt") do file
        seeds = Int[]
        currentRanges = RangeMap[]
        maps = Vector{RangeMap}[]

        for line in eachline(file)
            if occursin("map:", line)
                if !isempty(currentRanges)
                    push!(maps, currentRanges)
                    currentRanges = RangeMap[]
                end
            elseif startswith(line, "seeds:")
                seedStrs = split(line[7:end])
                append!(seeds, parse.(Int, seedStrs))
            elseif length(split(line)) == 3
                numbers = parse.(Int, split(line))
                push!(currentRanges, RangeMap(numbers[2], numbers[1], numbers[3]))
            end
        end
        push!(maps, currentRanges)

        minLocation = -1
        for seed in seeds
            location = seed
            for m in maps
                location = convertNumber(location, m)
            end
            if minLocation == -1 || location < minLocation
                minLocation = location
            end
        end

        println(minLocation)
    end
end

main()
