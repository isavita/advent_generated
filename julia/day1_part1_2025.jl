
function main()
    pos = 50
    zeros = 0
    open("input.txt") do io
        for raw in eachline(io)
            line = strip(raw)
            isempty(line) && continue
            dir = line[1]
            amount = parse(Int, SubString(line, 2))
            if dir == 'R'
                pos = mod(pos + amount, 100)
            else
                pos = mod(pos - amount, 100)
            end
            pos == 0 && (zeros += 1)
        end
    end
    println("The password is: $zeros")
end

main()
