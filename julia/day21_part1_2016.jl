
function solve_password_scrambling()
    operations = readlines("input.txt")
    password = collect("abcdefgh")

    for op in operations
        password = apply_operation(op, password)
    end

    println(join(password))
end

function apply_operation(op::String, password::Vector{Char})
    tokens = split(op)
    
    if tokens[1] == "swap"
        if tokens[2] == "position"
            x, y = parse(Int, tokens[3]) + 1, parse(Int, tokens[6]) + 1
            password[x], password[y] = password[y], password[x]
        else
            x, y = tokens[3][1], tokens[6][1]
            password = map(c -> c == x ? y : (c == y ? x : c), password)
        end
    elseif tokens[1] == "rotate"
        if tokens[2] == "left"
            steps = parse(Int, tokens[3])
            password = circshift(password, -steps)
        elseif tokens[2] == "right"
            steps = parse(Int, tokens[3])
            password = circshift(password, steps)
        else
            x = tokens[7][1]
            index = findfirst(==(x), password) - 1
            steps = 1 + index + (index >= 4 ? 1 : 0)
            password = circshift(password, steps)
        end
    elseif tokens[1] == "reverse"
        x, y = parse(Int, tokens[3]) + 1, parse(Int, tokens[5]) + 1
        password[x:y] = reverse(password[x:y])
    elseif tokens[1] == "move"
        x, y = parse(Int, tokens[3]) + 1, parse(Int, tokens[6]) + 1
        c = password[x]
        deleteat!(password, x)
        insert!(password, y, c)
    end

    return password
end

solve_password_scrambling()
