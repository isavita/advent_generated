# Read input from "input.txt"
masses = Int[]
open("input.txt", "r") do file
    for line in eachline(file)
        push!(masses, parse(Int, strip(line)))
    end
end

# Calculate the total fuel requirement
total = sum(floor(Int, m / 3) - 2 for m in masses)

# Print the answer
println(total)