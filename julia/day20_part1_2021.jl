
using Printf

function read_input(filename::String)
    lines = readlines(filename)
    algorithm = lines[1]
    image_lines = lines[3:end]
    rows = length(image_lines)
    cols = length(image_lines[1])
    image = Matrix{Char}(undef, rows, cols)
    for r in 1:rows
        for c in 1:cols
            image[r, c] = image_lines[r][c]
        end
    end
    return algorithm, image
end

function calculate_index(r::Int, c::Int, image::Matrix{Char}, flip::Bool)
    rows, cols = size(image)
    index = 0
    for dr in -1:1
        for dc in -1:1
            index <<= 1
            nr, nc = r + dr, c + dc
            if 1 <= nr <= rows && 1 <= nc <= cols
                if image[nr, nc] == '#'
                    index |= 1
                end
            elseif flip
                index |= 1
            end
        end
    end
    # Julia is 1-based, algorithm string index needs to be adjusted
    return index + 1
end

function apply_algorithm(image::Matrix{Char}, algorithm::String, flip::Bool)
    rows, cols = size(image)
    new_rows, new_cols = rows + 2, cols + 2
    enhanced_image = Matrix{Char}(undef, new_rows, new_cols)

    for r in 1:new_rows
        for c in 1:new_cols
            # Map new image coords (r, c) to the center of the 3x3 grid in the old image
            old_r, old_c = r - 1, c - 1
            algo_index = calculate_index(old_r, old_c, image, flip)
            enhanced_image[r, c] = algorithm[algo_index]
        end
    end
    return enhanced_image
end

function enhance_image(image::Matrix{Char}, algorithm::String, times::Int)
    # Check if the 'infinite' grid flips based on algorithm[0] and algorithm[511]
    # algorithm[1] in Julia corresponds to algorithm[0] in Python
    flips_infinite = algorithm[1] == '#' && algorithm[end] == '.'

    current_image = image
    for i in 1:times
        # Determine background state for out-of-bounds pixels
        # Iteration 1 (i=1): background is '.' (flip=false)
        # Iteration 2 (i=2): background is '#' if flips_infinite, else '.'
        # (flip = true if i is even and flips_infinite is true)
        flip = flips_infinite && (i % 2 == 0)
        current_image = apply_algorithm(current_image, algorithm, flip)
    end
    return current_image
end

function count_lit_pixels(image::Matrix{Char})
    return count(==('#'), image)
end

function main()
    algorithm, image = read_input("input.txt")
    final_image = enhance_image(image, algorithm, 2)
    result = count_lit_pixels(final_image)
    println(result)
end

main()
