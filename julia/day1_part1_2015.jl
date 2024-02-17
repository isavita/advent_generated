
open("input.txt") do file
    input = strip(read(file, String))
    floor = 0
    for c in input
        if c == '('
            floor += 1
        elseif c == ')'
            floor -= 1
        end
    end
    println(floor)
end
