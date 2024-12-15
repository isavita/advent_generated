
function solve()
    input = read("input.txt", String)
    total_sum = 0
    enabled = true
    for m in eachmatch(r"(mul\((\d{1,3}),(\d{1,3})\))|(do\(\))|(don't\(\))", input)
        if !isnothing(m.captures[1])
            if enabled
                x = parse(Int, m.captures[2])
                y = parse(Int, m.captures[3])
                total_sum += x * y
            end
        elseif !isnothing(m.captures[4])
            enabled = true
        elseif !isnothing(m.captures[5])
            enabled = false
        end
    end
    println(total_sum)
end

solve()
