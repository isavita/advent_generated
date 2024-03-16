open("input.txt", "r") do file
    groups = split(read(file, String), "\n\n")
    println(sum(length(unique(join(split(g, "\n")))) for g in groups))
end