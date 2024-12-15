
function solve()
    stones = split(strip(read("input.txt", String)))
    for _ in 1:25
        next_stones = String[]
        for s in stones
            if s == "0"
                push!(next_stones, "1")
            elseif iseven(length(s))
                mid = div(length(s), 2)
                left = lstrip(s[1:mid], '0')
                right = lstrip(s[mid+1:end], '0')
                push!(next_stones, isempty(left) ? "0" : left, isempty(right) ? "0" : right)
            else
                push!(next_stones, string(parse(Int, s) * 2024))
            end
        end
        stones = next_stones
    end
    println(length(stones))
end

solve()
