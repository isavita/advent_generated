
function generate_addresses(mask, address)
    floating = Int[]
    for (i, bit) in enumerate(mask)
        if bit == '1'
            address |= (1 << (36 - i))
        elseif bit == 'X'
            push!(floating, 36 - i)
        end
    end
    addresses = Int[]
    count = 1 << length(floating)
    for i in 0:(count-1)
        mod_address = address
        for (j, pos) in enumerate(floating)
            if (i & (1 << (j-1))) == 0
                mod_address &= ~(1 << pos)
            else
                mod_address |= (1 << pos)
            end
        end
        push!(addresses, mod_address)
    end
    return addresses
end

function main()
    open("input.txt", "r") do file
        mask = ""
        mem = Dict{Int, Int}()
        for line in eachline(file)
            if startswith(line, "mask = ")
                mask = line[8:end]
            else
                m = match(r"mem\[(\d+)] = (\d+)", line)
                address = parse(Int, m[1])
                value = parse(Int, m[2])
                addresses = generate_addresses(mask, address)
                for addr in addresses
                    mem[addr] = value
                end
            end
        end
        println(sum(values(mem)))
    end
end

main()
