
function calculate_max_joltage(bank::AbstractString)::Int
    for d1 in 9:-1:0
        ch = Char('0' + d1)
        idx = findfirst(==(ch), bank)
        if idx !== nothing && idx < lastindex(bank)
            maxd2 = maximum(c -> c - '0', filter(isdigit, @view bank[idx+1:end]), init=-1)
            maxd2 != -1 && return d1 * 10 + maxd2
        end
    end
    0
end

function main()
    total = sum(calculate_max_joltage, eachline("input.txt"))
    println("Total output joltage: ", total)
end

main()
