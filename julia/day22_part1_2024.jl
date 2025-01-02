
function nextSecret(s::UInt64)
    x = s * 0x40
    s ⊻= x
    s &= 0xFFFFFF
    x = s ÷ 0x20
    s ⊻= x
    s &= 0xFFFFFF
    x = s * 0x800
    s ⊻= x
    s &= 0xFFFFFF
    return s
end

function main()
    buyers = UInt64[]
    open("input.txt") do f
        for line in eachline(f)
            if !isempty(line)
                push!(buyers, parse(UInt64, line))
            end
        end
    end

    total = zero(UInt64)
    for b in buyers
        s = b
        for _ in 1:2000
            s = nextSecret(s)
        end
        total += s
    end
    println(total)
end

main()
