using Printf

function getPosition(row, column)
    return (row + column - 2) * (row + column - 1) ÷ 2 + column
end

function getCode(position)
    startCode = 20151125
    multiplier = 252533
    modulus = 33554393

    code = startCode
    for i in 1:position-1
        code = (code * multiplier) % modulus
    end
    return code
end

input = readline("input.txt")
m = match(r"row (\d+), column (\d+)", input)
row = parse(Int, m[1])
column = parse(Int, m[2])

pos = getPosition(row, column)
code = getCode(pos)

@printf "%d\n" code