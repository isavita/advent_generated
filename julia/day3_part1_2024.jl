
function solve()
    input = read("input.txt", String)
    total_sum = 0
    for match in eachmatch(r"mul\((\d{1,3}),(\d{1,3})\)", input)
        x = parse(Int, match.captures[1])
        y = parse(Int, match.captures[2])
        total_sum += x * y
    end
    println(total_sum)
end

solve()
