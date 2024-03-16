const SCREEN_WIDTH = 50
const SCREEN_HEIGHT = 6

function main()
    screen = falses(SCREEN_HEIGHT, SCREEN_WIDTH)
    for instruction in readlines("input.txt")
        process_instruction(instruction, screen)
    end
    display_screen(screen)
end

function display_screen(screen)
    for row in 1:SCREEN_HEIGHT
        for col in 1:SCREEN_WIDTH
            print(screen[row, col] ? "#" : ".")
        end
        println()
    end
end

function process_instruction(instruction, screen)
    if match(r"rect (\d+)x(\d+)", instruction) !== nothing
        a, b = parse.(Int, match(r"rect (\d+)x(\d+)", instruction).captures)
        rect(screen, a, b)
    elseif match(r"rotate row y=(\d+) by (\d+)", instruction) !== nothing
        a, b = parse.(Int, match(r"rotate row y=(\d+) by (\d+)", instruction).captures)
        rotate_row(screen, a + 1, b)
    elseif match(r"rotate column x=(\d+) by (\d+)", instruction) !== nothing
        a, b = parse.(Int, match(r"rotate column x=(\d+) by (\d+)", instruction).captures)
        rotate_column(screen, a + 1, b)
    end
end

function rect(screen, a, b)
    for y in 1:b, x in 1:a
        screen[y, x] = true
    end
end

function rotate_row(screen, row, shift)
    screen[row, :] = circshift(screen[row, :], shift)
end

function rotate_column(screen, col, shift)
    screen[:, col] = circshift(screen[:, col], shift)
end

main()