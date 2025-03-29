
using Printf

# --- Constants ---
const INPUT_FILE = "input.txt"

# --- Helper Functions ---

"""
    parse_input(filename::String)

Reads the input file and parses the enhancement algorithm and the initial image.
Returns the algorithm as a BitVector and the image as a BitMatrix.
'.' is represented as false, '#' as true.
"""
function parse_input(filename::String)
    lines = readlines(filename)

    # Algorithm: Convert '#' to true (1), '.' to false (0)
    algo_str = lines[1]
    if length(algo_str) != 512
        error("Algorithm string must be 512 characters long.")
    end
    algo = BitVector(c == '#' for c in algo_str)

    # Image: Skip blank line (line 2) and parse the grid
    image_lines = lines[3:end]
    if isempty(image_lines)
        error("Input image data is missing.")
    end
    height = length(image_lines)
    width = length(image_lines[1])
    image = falses(height, width) # Initialize BitMatrix with false ('.')

    for r in 1:height
        if length(image_lines[r]) != width
             error("Image rows have inconsistent widths.")
        end
        for c in 1:width
            image[r, c] = (image_lines[r][c] == '#')
        end
    end

    return algo, image
end

"""
    enhance_step(image::BitMatrix, algo::BitVector, background::Bool)

Performs one step of the image enhancement process.

Args:
    image: The current image state (BitMatrix).
    algo: The enhancement algorithm (BitVector).
    background: The state (true/false) of the infinite background pixels.

Returns:
    A tuple containing the new image (BitMatrix) and the new background state (Bool).
    The new image will be larger than the input image by 2 pixels in each dimension.
"""
function enhance_step(image::BitMatrix, algo::BitVector, background::Bool)
    rows, cols = size(image)

    # Pad the input image by 2 layers with the current background value.
    # This allows easy access to neighbors without boundary checks in the main loop.
    # Padding amount: 2 on each side means +4 rows/cols total.
    padded_rows, padded_cols = rows + 4, cols + 4
    padded_image = fill(background, padded_rows, padded_cols)

    # Copy current image into the center of the padded image (offset by 2 for padding)
    padded_image[3:rows+2, 3:cols+2] .= image

    # The output image dimensions will be original size + 2 (one layer added each side)
    # This corresponds to size(padded_image) - 2
    new_rows, new_cols = rows + 2, cols + 2
    new_image = falses(new_rows, new_cols)

    # Iterate through each pixel of the *new* image
    for r_out in 1:new_rows
        for c_out in 1:new_cols
            # Determine the 3x3 neighborhood in the *padded* input image.
            # The top-left corner of the 3x3 square in padded_image corresponds to (r_out, c_out).
            # Indices are r_pad_tl + (0 to 2), c_pad_tl + (0 to 2)
            r_pad_tl = r_out
            c_pad_tl = c_out

            index_val = 0
            # Read the 9 bits (top-to-bottom, left-to-right)
            for dr in 0:2
                for dc in 0:2
                    bit = padded_image[r_pad_tl + dr, c_pad_tl + dc]
                    index_val = (index_val << 1) | bit
                end
            end

            # Get the output pixel value from the algorithm (adjust for 1-based indexing)
            new_image[r_out, c_out] = algo[index_val + 1]
        end
    end

    # Determine the new state of the infinite background
    # If current background is 0 (.), index is 000000000 = 0 -> algo[1]
    # If current background is 1 (#), index is 111111111 = 511 -> algo[512]
    new_background = background ? algo[512] : algo[1]

    return new_image, new_background
end

"""
    solve(filename::String, steps::Int)

Solves the Trench Map challenge for the given number of steps.
"""
function solve(filename::String, steps::Int)
    algo, image = parse_input(filename)
    background = false # Initially, the infinite background is dark ('.')

    # Apply the enhancement process for the specified number of steps
    for _ in 1:steps
        image, background = enhance_step(image, algo, background)
    end

    # The puzzle asks for the count of lit pixels ('#') in the final image.
    # Even if the background is lit, we count only the pixels in our finite grid.
    return count(image) # count(true, image) also works
end

# --- Main Execution ---

"""
    main()

Main function to run the solution. Reads input, solves parts 1 and 2, and prints results.
"""
function main()
    # Part 1: Apply enhancement 2 times
    result_part1 = solve(INPUT_FILE, 2)
    println("Part 1: ", result_part1)

    # Part 2: Apply enhancement 50 times
    result_part2 = solve(INPUT_FILE, 50)
    println("Part 2: ", result_part2)
end

# Execute the main function
main()
