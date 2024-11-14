
using JSON

function read_input(file)
    open(file, "r") do f
        read(f, String)
    end
end

function compare(a, b)
    if typeof(a) == Int && typeof(b) == Int
        return sign(a - b)
    elseif typeof(a) == Int
        return compare([a], b)
    elseif typeof(b) == Int
        return compare(a, [b])
    else
        for i in 1:min(length(a), length(b))
            c = compare(a[i], b[i])
            if c != 0
                return c
            end
        end
        return sign(length(a) - length(b))
    end
end

function main()
    s = read_input("input.txt")
    packets = []
    for pair in split(s, "\n\n")
        sp = split(pair, "\n")
        push!(packets, JSON.parse(sp[1]))
        push!(packets, JSON.parse(sp[2]))
    end
    divider1 = [[2]]
    divider2 = [[6]]
    push!(packets, divider1, divider2)
    sort!(packets, lt=(a, b) -> compare(a, b) < 0)
    divider1Pos = findfirst(x -> compare(x, divider1) >= 0, packets)
    divider2Pos = findfirst(x -> compare(x, divider2) >= 0, packets)
    println((divider1Pos * divider2Pos))
end

main()
