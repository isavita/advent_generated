
function trim_leading_zeros(s::String)
    i = 1
    while i < length(s) && s[i] == '0'
        i += 1
    end
    return s[i:end]
end

function split_stone(s::String)
    mid = div(length(s), 2)
    left = trim_leading_zeros(s[1:mid])
    right = trim_leading_zeros(s[mid+1:end])
    if isempty(left)
        left = "0"
    end
    if isempty(right)
        right = "0"
    end
    return left, right
end

function multiply_by_2024(s::String)
    num = parse.(Int, collect(s))
    multiplier = [2, 0, 2, 4]
    result = zeros(Int, length(num) + length(multiplier))
    for i in length(num):-1:1
        carry = 0
        for j in length(multiplier):-1:1
            product = num[i] * multiplier[j] + result[i+j] + carry
            result[i+j] = product % 10
            carry = div(product, 10)
        end
        result[i] += carry
    end
    start = 1
    while start < length(result) && result[start] == 0
        start += 1
    end
    return join(string.(result[start:end]))
end

function solve()
    input = open("input.txt", "r") do file
        readline(file)
    end
    stones_str = split(input)
    stones_map = Dict{String, Int64}()
    for s in stones_str
        stones_map[s] = get(stones_map, s, 0) + 1
    end
    steps = 75
    for _ in 1:steps
        new_stones_map = Dict{String, Int64}()
        for (stone, count) in stones_map
            if stone == "0"
                new_stones_map["1"] = get(new_stones_map, "1", 0) + count
            elseif length(stone) % 2 == 0
                left, right = split_stone(stone)
                new_stones_map[left] = get(new_stones_map, left, 0) + count
                new_stones_map[right] = get(new_stones_map, right, 0) + count
            else
                new_stone = multiply_by_2024(stone)
                new_stones_map[new_stone] = get(new_stones_map, new_stone, 0) + count
            end
        end
        stones_map = new_stones_map
    end
    total_stones = sum(values(stones_map))
    println(total_stones)
end

solve()
