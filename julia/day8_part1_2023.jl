const ELEM_TO_MATCH = "ZZZ"

function main()
    input = read("input.txt", String)
    input = strip(input)

    regex = r"[A-Z]{3}"
    lines = split(input, "\n")

    desert_map = Dict{String,Tuple{String,String}}()

    # Ignore the first two lines since they are the instruction set and a blank line
    for line in lines[3:end]
        if isempty(line)
            continue
        end

        matches = collect(eachmatch(regex, line))
        desert_map[matches[1].match] = (matches[2].match, matches[3].match)
    end

    current = "AAA"
    steps = 0

    while current != ELEM_TO_MATCH
        for direction in lines[1]
            if direction == 'R'
                current = desert_map[current][2]
            elseif direction == 'L'
                current = desert_map[current][1]
            end
            steps += 1

            if current == ELEM_TO_MATCH
                break
            end
        end
    end

    println(steps)
end

main()