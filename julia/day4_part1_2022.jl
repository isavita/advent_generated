using DelimitedFiles

function parse_range(r::SubString{String})
    parts = split(r, "-")
    start = parse(Int, String(parts[1]))
    endd = parse(Int, String(parts[2]))
    return start, endd
end

function main()
    lines = readdlm("input.txt", '\n', String)
    count = 0
    for line in lines
        ranges = split(line, ",")
        if length(ranges) != 2
            continue
        end
        start1, end1 = parse_range(ranges[1])
        start2, end2 = parse_range(ranges[2])
        if (start1 <= start2 && end1 >= end2) || (start2 <= start1 && end2 >= end1)
            count += 1
        end
    end
    println(count)
end

main()