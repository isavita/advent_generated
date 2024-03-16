function from_snafu(s::String)::Int
    n = 0
    for c in s
        n *= 5
        if c == '='
            n -= 2
        elseif c == '-'
            n -= 1
        else
            n += parse(Int, c)
        end
    end
    return n
end

function to_snafu(n::Int)::String
    b = Char[]
    while n > 0
        switch = n % 5
        if switch == 3
            n += 5
            push!(b, '=')
        elseif switch == 4
            n += 5
            push!(b, '-')
        else
            push!(b, Char('0' + switch))
        end
        n ÷= 5
    end
    return String(reverse(b))
end

function main()
    sum = 0
    open("input.txt", "r") do file
        for line in eachline(file)
            sum += from_snafu(line)
        end
    end
    println(to_snafu(sum))
end

main()