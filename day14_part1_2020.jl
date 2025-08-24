using Printf

function apply_mask(value::Int, mask::String)
    result = 0
    for i in 1:36
        bit_value = 1 << (35 - i + 1)
        if mask[i] == '1'
            result |= bit_value
        elseif mask[i] == 'X'
            result |= (value & bit_value)
        end
    end
    return result
end

function main()
    mask = ""
    mem = Dict{Int,Int}()

    open("input.txt") do file
        for line in eachline(file)
            if startswith(line, "mask = ")
                mask = line[8:end]
            else
                m = match(r"mem\[(\d+)\] = (\d+)", line)
                if m !== nothing
                    address = parse(Int, m.captures[1])
                    value = parse(Int, m.captures[2])
                    mem[address] = apply_mask(value, mask)
                end
            end
        end
    end

    sum = 0
    for value in values(mem)
        sum += value
    end

    @printf "%d\n" sum
end

main()