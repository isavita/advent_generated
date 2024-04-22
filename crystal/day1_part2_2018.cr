# Open the input file and read the data
file = File.open("input.txt", "r")
data = file.gets_to_end.chomp
file.close

# Split the data into an array of frequency changes
frequency_changes = data.split("\n")

# Initialize a hash to store the frequencies
frequencies = Hash(Int32, Bool).new

# Initialize the current frequency
current_frequency = 0

# Add the current frequency to the hash
frequencies[current_frequency] = true

# Loop indefinitely
loop do
  # Iterate over each frequency change
  frequency_changes.each do |change|
    # Convert the change to an integer
    frequency_delta = change.to_i

    # Add the change to the current frequency
    current_frequency += frequency_delta

    # Check if the frequency has been seen before
    if frequencies.has_key?(current_frequency)
      # If it has, print the frequency and exit
      puts current_frequency
      exit
    end

    # Add the current frequency to the hash
    frequencies[current_frequency] = true
  end
end