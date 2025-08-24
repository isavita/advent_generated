# Function to check if a number meets the password criteria
function is_valid_password(n)
    s = string(n)
    has_double = false
    always_increases = true
    prev_char = '0'

    # Check for increasing sequence and at least one double
    for i in 1:length(s)
        if s[i] < prev_char
            always_increases = false
            break
        end
        if i > 1 && s[i] == s[i-1]
            has_double = true
        end
        prev_char = s[i]
    end

    return has_double && always_increases
end

# Function to check the new criteria for Part Two
function is_valid_password_part_two(n)
    s = string(n)
    always_increases = true
    prev_char = '0'
    groups = Dict{Char, Int}()

    # Count occurrences of each digit
    for c in s
        if c < prev_char
            always_increases = false
            break
        end
        groups[c] = get(groups, c, 0) + 1
        prev_char = c
    end

    # Check for a group of exactly two
    has_group_of_two = any(v == 2 for v in values(groups))

    return has_group_of_two && always_increases
end

# Read input range from file
function read_input(filename)
    open(filename, "r") do file
        range = readline(file)
        parse.(Int, split(range, "-"))
    end
end

# Main function to solve both parts
function solve_challenge(filename)
    start_range, end_range = read_input(filename)
    valid_count_part_one = 0
    valid_count_part_two = 0

    for n in start_range:end_range
        if is_valid_password(n)
            valid_count_part_one += 1
        end
        if is_valid_password_part_two(n)
            valid_count_part_two += 1
        end
    end

    println("Part One: Valid passwords count = ", valid_count_part_one)
    println("Part Two: Valid passwords count = ", valid_count_part_two)
end

# Call the main function with the input file
solve_challenge("input.txt")