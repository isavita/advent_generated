
using JSON

function main()
    packets = read_all("input.txt")
    sum = 0
    for (i, pair) in enumerate(packets)
        first, second = pair
        if compare(first, second) == -1
            sum += i
        end
    end
    println(sum)
end

function read_all(path)
    content = read(path, String)
    return [parse_packet(pair) for pair in split(content, "\n\n")]
end

function parse_packet(pair)
    return [JSON.parse(line) for line in split(pair, "\n")]
end

function compare(a, b)
    if isa(a, Number) && isa(b, Number)
        return sign(a - b)
    elseif isa(a, Number)
        return compare([a], b)
    elseif isa(b, Number)
        return compare(a, [b])
    else
        for (x, y) in zip(a, b)
            cmp = compare(x, y)
            if cmp != 0
                return cmp
            end
        end
        return sign(length(a) - length(b))
    end
end

function sign(n)
    return n < 0 ? -1 : n > 0 ? 1 : 0
end

main()
