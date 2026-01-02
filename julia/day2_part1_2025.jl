
using Base: parse, read, replace, split, isfile, isempty, println

function sum_invalid_ids_in_range(L::BigInt, R::BigInt)::BigInt
    total = BigInt(0)
    k = 1
    while true
        p10 = big(10)^k
        mult = p10 + 1
        minS = big(10)^(k - 1)
        maxS = p10 - 1
        if minS * mult > R
            break
        end
        sMin = (L + mult - 1) ÷ mult
        sMax = R ÷ mult
        a = max(sMin, minS)
        b = min(sMax, maxS)
        if a <= b
            cnt = b - a + 1
            sumS = cnt * (a + b) ÷ 2
            total += sumS * mult
        end
        k += 1
    end
    total
end

function main()
    filename = "input.txt"
    !isfile(filename) && return
    content = replace(read(filename, String), r"\s+" => "")
    isempty(content) && return
    total = BigInt(0)
    for rng in split(content, ",")
        isempty(rng) && continue
        b = split(rng, "-")
        length(b) != 2 && continue
        low = parse(BigInt, b[1])
        high = parse(BigInt, b[2])
        total += sum_invalid_ids_in_range(low, high)
    end
    println(total)
end

main()
