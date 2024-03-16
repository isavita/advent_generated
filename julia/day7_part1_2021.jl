open("input.txt") do f
    positions = parse.(Int, split(read(f, String), ","))
    fuel = Inf
    for p in minimum(positions):maximum(positions)
        f = sum(abs(x-p) for x in positions)
        fuel = min(fuel, f)
    end
    println(fuel)
end