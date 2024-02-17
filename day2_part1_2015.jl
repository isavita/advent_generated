function main()
    file = open("input.txt", "r")
    total_paper = 0
    for line in eachline(file)
        dimensions = split(line, 'x')
        l, w, h = parse.(Int, dimensions)
        side_areas = [l*w, w*h, h*l]
        paper = 2*sum(side_areas) + minimum(side_areas)
        total_paper += paper
    end
    close(file)
    println(total_paper)
end

main()
