
function count_twos_and_threes(id)
    char_count = Dict{Char, Int}()
    for char in id
        char_count[char] = get(char_count, char, 0) + 1
    end
    has_twos = any(x -> x == 2, values(char_count))
    has_threes = any(x -> x == 3, values(char_count))
    return has_twos, has_threes
end

function main()
    file = open("input.txt")
    two_count, three_count = 0, 0
    for line in eachline(file)
        has_twos, has_threes = count_twos_and_threes(line)
        if has_twos
            two_count += 1
        end
        if has_threes
            three_count += 1
        end
    end
    close(file)
    checksum = two_count * three_count
    println(checksum)
end

main()
