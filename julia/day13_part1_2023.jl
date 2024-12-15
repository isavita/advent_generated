
function solve()
    mirrors = parse_input()
    res = 0
    for mirror in mirrors
        res += get_mirror_axis(mirror.cols)
        res += get_mirror_axis(mirror.rows) * 100
    end
    println(res)
end

function parse_input()
    input = readlines("input.txt")
    mirrors = []
    mirror_str = []
    for line in input
        if isempty(line)
            push!(mirrors, parse_mirror(mirror_str))
            mirror_str = []
        else
            push!(mirror_str, line)
        end
    end
    push!(mirrors, parse_mirror(mirror_str))
    return mirrors
end

function parse_mirror(mirror_str)
    rows = zeros(Int, length(mirror_str))
    cols = zeros(Int, length(mirror_str[1]))
    for (y, line) in enumerate(mirror_str)
        for (x, char) in enumerate(line)
            rows[y] <<= 1
            cols[x] <<= 1
            if char == '#'
                rows[y] += 1
                cols[x] += 1
            end
        end
    end
    return (rows=rows, cols=cols)
end

function get_mirror_axis(lines)
    for i in 1:length(lines)-1
        is_mirror = true
        for j in 0:min(i-1, length(lines)-i-1)
            if lines[i-j] != lines[i+1+j]
                is_mirror = false
                break
            end
        end
        if is_mirror
            return i
        end
    end
    return 0
end

solve()
