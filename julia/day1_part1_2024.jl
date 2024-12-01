
function solve()
    try
        open("input.txt") do f
            left = Int[]
            right = Int[]
            for line in eachline(f)
                fields = split(line)
                if length(fields) != 2
                    error("Invalid input format: expected 2 numbers per line")
                end
                push!(left, parse(Int, fields[1]))
                push!(right, parse(Int, fields[2]))
            end
            if length(left) != length(right)
                error("Lists have different lengths")
            end
            sort!(left)
            sort!(right)
            total_distance = sum(abs.(left .- right))
            println(total_distance)
        end
    catch e
        println("Error: ", e)
    end
end

solve()
