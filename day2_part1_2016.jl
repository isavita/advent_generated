
function get_bathroom_code(instructions)
    keypad = [
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9],
    ]
    x, y = 1, 1 # Start at '5'
    code = ""

    for instruction in instructions
        for move in instruction
            if move == 'U' && x > 1
                x -= 1
            elseif move == 'D' && x < 3
                x += 1
            elseif move == 'L' && y > 1
                y -= 1
            elseif move == 'R' && y < 3
                y += 1
            end
        end
        code *= string(keypad[x][y])
    end

    return code
end

open("input.txt") do file
    instructions = readlines(file)
    code = get_bathroom_code(instructions)
    println(code)
end
