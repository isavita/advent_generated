function main()
    file = open("input.txt", "r")
    lantern_fish_counts = zeros(Int, 9)
    for age in split(readline(file), ",")
        lantern_fish_counts[parse(Int, age) + 1] += 1
    end
    close(file)

    for _ in 1:256
        new_lantern_fish = lantern_fish_counts[1]
        lantern_fish_counts[1:8] = lantern_fish_counts[2:9]
        lantern_fish_counts[7] += new_lantern_fish
        lantern_fish_counts[9] = new_lantern_fish
    end

    println(sum(lantern_fish_counts))
end

main()