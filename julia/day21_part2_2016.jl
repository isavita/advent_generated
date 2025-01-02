
function swap_positions!(pw, x, y)
    pw[x+1], pw[y+1] = pw[y+1], pw[x+1]
end

function swap_letters!(pw, x, y)
    swap_positions!(pw, findfirst(isequal(x), pw) - 1, findfirst(isequal(y), pw) - 1)
end

function rotate!(pw, steps)
    len = length(pw)
    steps = mod(steps, len)
    if steps < 0
        steps += len
    end
    pw[:] = [pw[len-steps+1:end]; pw[1:len-steps]]
end

function rotate_letter!(pw, x)
    index = findfirst(isequal(x), pw) - 1
    if index >= 4
        index += 1
    end
    rotate!(pw, index + 1)
end

function derotate_letter!(pw, x)
    index = findfirst(isequal(x), pw) - 1
    rot = if index % 2 == 1
        -(index + 1) ÷ 2
    elseif index != 0
        (6 - index) ÷ 2
    else
        -1
    end
    rotate!(pw, rot)
end

function reverse_sub!(pw, x, y)
    pw[x+1:y+1] = pw[x+1:y+1][end:-1:1]
end

function move!(pw, x, y)
    ch = pw[x+1]
    deleteat!(pw, x+1)
    insert!(pw, y+1, ch)
end

function scramble!(pw, instructions, direction)
    if direction < 0
        instructions = reverse(instructions)
    end
    for instruction in instructions
        line = split(instruction)
        if startswith(instruction, "swap")
            x, y = line[3], line[end]
            if line[2] == "position"
                xi, yi = parse(Int, x), parse(Int, y)
                swap_positions!(pw, xi, yi)
            else
                swap_letters!(pw, x[1], y[1])
            end
        elseif startswith(instruction, "rotate")
            if line[2] == "based"
                if direction > 0
                    rotate_letter!(pw, line[end][1])
                else
                    derotate_letter!(pw, line[end][1])
                end
            else
                x = parse(Int, line[3])
                if line[2] == "left"
                    x = -x
                end
                if direction < 0
                    x = -x
                end
                rotate!(pw, x)
            end
        elseif startswith(instruction, "reverse")
            x, y = line[3], line[end]
            xi, yi = parse(Int, x), parse(Int, y)
            reverse_sub!(pw, xi, yi)
        elseif startswith(instruction, "move")
            x, y = line[3], line[end]
            xi, yi = parse(Int, x), parse(Int, y)
            if direction < 0
                xi, yi = yi, xi
            end
            move!(pw, xi, yi)
        end
    end
    return pw
end

function unscramble!(pw, instructions)
    return scramble!(pw, instructions, -1)
end

instructions = readlines("input.txt")
hashed = "fbgdceah"
pw = collect(hashed)
result = unscramble!(pw, instructions)
println(join(result))
