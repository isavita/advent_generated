# Function to perform the modified dragon curve transformation
function dragon_curve(a::String)
    b = reverse([c == '0' ? '1' : '0' for c in a])
    return a * "0" * join(b)
end

# Function to compute the checksum
function compute_checksum(data::String)
    checksum = data
    while length(checksum) % 2 == 0
        pairs = [checksum[i:i+1] for i in 1:2:length(checksum)-1]
        checksum = join([p[1] == p[2] ? '1' : '0' for p in pairs])
    end
    return checksum
end

# Function to process the disk filling and checksum calculation
function process_disk_filling(initial_state::String, disk_length::Int)
    data = initial_state
    while length(data) < disk_length
        data = dragon_curve(data)
    end
    data = data[1:disk_length]
    checksum = compute_checksum(data)
    return checksum
end

# Main function to read input and output the result
function main()
    # Read the initial state from the input file
    input_file = "input.txt"
    initial_state = String(strip(read(input_file, String)))  # Explicitly convert to String

    # The disk length to fill
    disk_length = 272

    # Process the disk filling and checksum calculation
    checksum = process_disk_filling(initial_state, disk_length)

    # Print the checksum to standard output
    println(checksum)
end

# Run the main function
main()