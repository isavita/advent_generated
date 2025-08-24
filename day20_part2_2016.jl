function read_and_parse_input(filename)
    ranges = []
    open(filename, "r") do file
        for line in eachline(file)
            start_ip, end_ip = split(line, '-')
            push!(ranges, (parse(Int, start_ip), parse(Int, end_ip)))
        end
    end
    return ranges
end

function merge_ranges(ranges)
    sort!(ranges, by = x -> x[1])
    merged = []
    current_start, current_end = ranges[1]

    for (start, end_) in ranges
        if start <= current_end + 1
            current_end = max(current_end, end_)
        else
            push!(merged, (current_start, current_end))
            current_start, current_end = start, end_
        end
    end
    push!(merged, (current_start, current_end))
    return merged
end

function find_lowest_unblocked_ip(merged_ranges)
    if merged_ranges[1][1] > 0
        return 0
    end

    for i in 1:length(merged_ranges)-1
        if merged_ranges[i][2] + 1 < merged_ranges[i+1][1]
            return merged_ranges[i][2] + 1
        end
    end
end

function count_allowed_ips(merged_ranges)
    allowed_count = 0
    max_ip = 4294967295

    for i in 1:length(merged_ranges)-1
        allowed_count += merged_ranges[i+1][1] - merged_ranges[i][2] - 1
    end

    if merged_ranges[end][2] < max_ip
        allowed_count += max_ip - merged_ranges[end][2]
    end

    return allowed_count
end

function main()
    ranges = read_and_parse_input("input.txt")
    merged_ranges = merge_ranges(ranges)
    lowest_unblocked_ip = find_lowest_unblocked_ip(merged_ranges)
    total_allowed_ips = count_allowed_ips(merged_ranges)

    println("The lowest unblocked IP is: ", lowest_unblocked_ip)
    println("The total number of allowed IPs is: ", total_allowed_ips)
end

main()