
function count_valid_passwords(filename::String)
    valid_count = 0

    # Open the file and read each line
    open(filename, "r") do file
        for line in eachline(file)
            # Split the line into policy and password
            policy, password = split(line, ": ")
            range, char = split(policy, " ")
            min_count, max_count = parse.(Int, split(range, "-"))

            # Count occurrences of the character in the password
            char_count = count(c -> c == char[1], password)

            # Check if the password is valid
            if min_count <= char_count <= max_count
                valid_count += 1
            end
        end
    end

    return valid_count
end

# Read from input.txt and print the number of valid passwords
println("Number of valid passwords: ", count_valid_passwords("input.txt"))
