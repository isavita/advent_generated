adapters = parse.(Int, readlines("input.txt"))
sorted_adapters = sort([0; adapters; maximum(adapters) + 3])
diffs = diff(sorted_adapters)
println(count(x -> x == 1, diffs) * count(x -> x == 3, diffs))