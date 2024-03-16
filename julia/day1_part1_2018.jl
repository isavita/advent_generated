function read_input(filename::String)
    lines = String[]
    open(filename, "r") do file
        for line in eachline(file)
            push!(lines, line)
        end
    end
    return lines
end

function parse_change(change::String)
    sign = 1
    if change[1] == '-'
        sign = -1
        change = change[2:end]
    end
    num = parse(Int, change)
    return sign * num
end

function main()
    freq_changes = read_input("input.txt")
    freq = 0
    for change in freq_changes
        freq += parse_change(change)
    end
    println(freq)
end

main()