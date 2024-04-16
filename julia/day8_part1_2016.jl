# Define the dimensions of the screen
const WIDTH = 50
const HEIGHT = 6

# Initialize the screen with all pixels turned off
function create_screen()
    return falses(HEIGHT, WIDTH)
end

# Parse the instruction and apply the corresponding operation
function parse_instruction(screen, instruction)
    if occursin("rect", instruction)
        a, b = match(r"rect (\d+)x(\d+)", instruction).captures
        for i in 1:parse(Int, b), j in 1:parse(Int, a)
            screen[i, j] = true
        end
    elseif occursin("rotate row", instruction)
        y, b = match(r"rotate row y=(\d+) by (\d+)", instruction).captures
        row = parse(Int, y) + 1
        shift = parse(Int, b)
        screen[row, :] = circshift(screen[row, :], shift)
    elseif occursin("rotate column", instruction)
        x, b = match(r"rotate column x=(\d+) by (\d+)", instruction).captures
        col = parse(Int, x) + 1
        shift = parse(Int, b)
        screen[:, col] = circshift(screen[:, col], shift)
    end
end

# Read the input file and process each instruction
function process_instructions(filename)
    screen = create_screen()
    open(filename, "r") do file
        for line in eachline(file)
            parse_instruction(screen, line)
        end
    end
    return screen
end

# Count the number of lit pixels
function count_lit_pixels(screen)
    return count(screen)
end

# Main function to run the process
function main()
    screen = process_instructions("input.txt")
    println("Number of lit pixels: ", count_lit_pixels(screen))
end

main()