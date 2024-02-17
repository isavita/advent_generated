
using DelimitedFiles

function main()
    data = readlines("input.txt")
    data = join(data, "")
    steps = parse(Int, data)
    buffer = [0]
    current_pos = 1

    for i in 1:2017
        current_pos = (current_pos + steps) % length(buffer)
        insert!(buffer, current_pos + 1, i)
        current_pos += 1
    end

    for (i, val) in enumerate(buffer)
        if val == 2017
            println(buffer[(i + 1) % length(buffer)])
            break
        end
    end
end

main()
