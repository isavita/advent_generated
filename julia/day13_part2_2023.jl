
function read_patterns(filename)
    patterns = Vector{Vector{String}}()
    current_pattern = String[]
    
    for line in readlines(filename)
        if isempty(line)
            push!(patterns, current_pattern)
            current_pattern = String[]
        else
            push!(current_pattern, line)
        end
    end
    
    # Add the last pattern
    if !isempty(current_pattern)
        push!(patterns, current_pattern)
    end
    
    return patterns
end

function find_reflection(pattern, ignore_original = -1)
    rows = length(pattern)
    
    # Check horizontal reflections
    for r in 1:rows-1
        if r == ignore_original
            continue
        end
        
        is_reflection = true
        for offset in 0:min(r-1, rows-r-1)
            if pattern[r-offset] != pattern[r+offset+1]
                is_reflection = false
                break
            end
        end
        
        if is_reflection
            return r
        end
    end
    
    return 0
end

function transpose_pattern(pattern)
    return [join(row[c] for row in pattern) for c in 1:length(pattern[1])]
end

function solve_part1(patterns)
    total = 0
    
    for pattern in patterns
        # Check horizontal reflection
        horizontal_reflection = find_reflection(pattern)
        if horizontal_reflection > 0
            total += 100 * horizontal_reflection
        else
            # Check vertical reflection
            vertical_reflection = find_reflection(transpose_pattern(pattern))
            total += vertical_reflection
        end
    end
    
    return total
end

function solve_part2(patterns)
    total = 0
    
    for pattern in patterns
        original_horizontal = find_reflection(pattern)
        original_vertical = find_reflection(transpose_pattern(pattern))
        
        rows, cols = length(pattern), length(pattern[1])
        found_new_reflection = false
        
        for r in 1:rows
            for c in 1:cols
                # Create a copy of the pattern and flip the character
                modified_pattern = deepcopy(pattern)
                modified_pattern[r] = modified_pattern[r][1:c-1] * 
                    (modified_pattern[r][c] == '.' ? '#' : '.') * 
                    modified_pattern[r][c+1:end]
                
                # Check horizontal reflection
                horizontal_reflection = find_reflection(modified_pattern, original_horizontal)
                if horizontal_reflection > 0 && horizontal_reflection != original_horizontal
                    total += 100 * horizontal_reflection
                    found_new_reflection = true
                    break
                end
                
                # Check vertical reflection
                modified_vertical_pattern = transpose_pattern(modified_pattern)
                vertical_reflection = find_reflection(modified_vertical_pattern, original_vertical)
                if vertical_reflection > 0 && vertical_reflection != original_vertical
                    total += vertical_reflection
                    found_new_reflection = true
                    break
                end
            end
            
            if found_new_reflection
                break
            end
        end
    end
    
    return total
end

function main()
    patterns = read_patterns("input.txt")
    
    part1_result = solve_part1(patterns)
    println("Part 1 result: ", part1_result)
    
    part2_result = solve_part2(patterns)
    println("Part 2 result: ", part2_result)
end

main()
