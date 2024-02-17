
open("input.txt") do file
    genAStart = parse(Int64, readline(file))
    genBStart = parse(Int64, readline(file))

    genAFactor = 16807
    genBFactor = 48271
    modulus = 2147483647

    genA = genAStart
    genB = genBStart
    matches = 0

    for i in 1:5000000
        while true
            genA = (genA * genAFactor) % modulus
            if genA % 4 == 0
                break
            end
        end

        while true
            genB = (genB * genBFactor) % modulus
            if genB % 8 == 0
                break
            end
        end

        if genA & 0xFFFF == genB & 0xFFFF
            matches += 1
        end
    end

    println(matches)
end
