
open("input.txt") do file
    totalRibbon = 0
    for line in eachline(file)
        dimensions = split(line, "x")
        l, w, h = parse.(Int, dimensions)

        bow = l * w * h

        sides = [l, w, h]
        wrap = 2 * sum(sort(sides)[1:2])

        totalRibbon += bow + wrap
    end
    println(totalRibbon)
end
