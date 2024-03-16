input = readlines("input.txt")
layers = Dict(parse.(Int, split(line, ": ")) for line in input)
severity = sum(k * v for (k, v) in layers if k % (2*(v-1)) == 0)
println(severity)