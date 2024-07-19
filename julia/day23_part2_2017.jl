
function is_prime(n)
    for i in 2:floor(sqrt(n))
        if n % i == 0
            return false
        end
    end
    return true
end

function main()
    b = 57 * 100 + 100000
    c = b + 17000
    h = 0

    for x in b:17:c
        if !is_prime(x)
            h += 1
        end
    end

    println(h)
end

main()
