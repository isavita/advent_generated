# Function to generate data using the modified dragon curve
function generate_data(initial_state::String, target_length::Int)
    data = initial_state
    while length(data) < target_length
        b = replace(reverse(data), '0' => '1', '1' => '0')
        data = data * "0" * b
    end
    return data[1:target_length]
end

# Function to calculate the checksum
function calculate_checksum(data::String)
    checksum = data
    while true
        checksum = join([checksum[i] == checksum[i+1] ? '1' : '0' for i in 1:2:length(checksum)-1])
        if isodd(length(checksum))
            break
        end
    end
    return checksum
end

# Read the initial state from file and convert to String
function read_input(filename::String)
    return String(strip(read(filename, String)))
end

# Main function to process the input and output the checksum
function process_disk(filename::String, disk_length::Int)
    initial_state = read_input(filename)
    data = generate_data(initial_state, disk_length)
    checksum = calculate_checksum(data)
    println("Checksum for disk length $disk_length: $checksum")
end

# Process both parts of the challenge
process_disk("input.txt", 272)       # Part 1
process_disk("input.txt", 35651584)  # Part 2