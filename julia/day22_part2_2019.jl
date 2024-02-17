
const Size = 119315717514047

struct Deck
    step::Int
    direction::Int
    top::Int
end

function deal_into_new_stack(d::Deck)
    d.top = (d.top - d.direction * d.step + Size) % Size
    d.direction *= -1
end

function cut_n(d::Deck, n::Int)
    d.top = (d.top + d.direction * d.step * n + Size) % Size
end

function deal_with_increment_n(d::Deck, n::Int)
    inv = modinv(n, Size)
    d.step *= inv
    d.direction *= inv
end

function pick(d::Deck, n::Int)
    current = d.top
    for i in 1:n
        current = ((current + d.direction * d.step) % Size + Size) % Size
    end
    return current
end

function new_deck()
    return Deck(1, 1, 0)
end

function main()
    size = BigInt(Size)
    iter = BigInt(101741582076661)
    offset = BigInt(0)
    increment = BigInt(1)

    file = open("input.txt")
    for line in eachline(file)
        if line == "deal into new stack"
            increment *= BigInt(-1)
            offset += increment
            continue
        end

        if occursin("cut", line)
            n = parse(Int, split(line)[2])
            offset += BigInt(n) * increment
            continue
        end

        if occursin("deal with increment", line)
            n = parse(Int, split(line)[end])
            increment *= modinv(BigInt(n), size)
            continue
        end
    end

    final_incr = powermod(increment, iter, size)

    final_offs = (offset * (1 - final_incr) * modinv(1 - increment, size)) % size

    answer = (2020 * final_incr + final_offs) % size

    println(answer)
end

function egcd(a, b)
    if a == 0
        return b, 0, 1
    end
    gcd, y, x = egcd(b % a, a)
    return gcd, x - (b ÷ a) * y, y
end

function modinv(a, m)
    g, x, _ = egcd(a, m)
    if g != 1
        error("modular inverse does not exist")
    end
    if x < 0
        x += m
    end
    return x % m
end

main()
