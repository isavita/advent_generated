
const mod = 1 << 24
const numSteps = 2000

nextSecret(s) = begin
    x = s * 64
    s ⊻= x
    s &= mod - 1
    x = s ÷ 32
    s ⊻= x
    s &= mod - 1
    x = s * 2048
    s ⊻= x
    s &= mod - 1
    s
end

encodeChange4(c1, c2, c3, c4) = (c1+9) + (c2+9)*19 + (c3+9)*19^2 + (c4+9)*19^3

function main()
    initials = parse.(UInt64, filter(!isempty, readlines("input.txt")))

    buyers = map(initials) do initVal
        prices = zeros(Int, numSteps + 1)
        s = initVal
        for j in 1:numSteps+1
            prices[j] = Int(s % 10)
            j <= numSteps && (s = nextSecret(s))
        end
        changes = prices[2:end] - prices[1:end-1]
        (prices, changes)
    end

    patternCount = 19^4
    globalSum = zeros(Int64, patternCount)

    for (prices, changes) in buyers
        localPrice = fill(-1, patternCount)
        for i in 1:numSteps-3
            c1, c2, c3, c4 = changes[i], changes[i+1], changes[i+2], changes[i+3]
            if any(x -> abs(x) > 9, (c1, c2, c3, c4))
                continue
            end
            idx = encodeChange4(c1, c2, c3, c4)
            if localPrice[idx] < 0
                localPrice[idx] = prices[i+4]
            end
        end
        for (idx, p) in enumerate(localPrice)
            p >= 0 && (globalSum[idx] += p)
        end
    end

    println(maximum(globalSum))
end

main()
