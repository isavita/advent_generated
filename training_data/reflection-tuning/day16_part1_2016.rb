def generate_data(initial_state, length)
  data = initial_state.dup
  while data.length < length
    b = data.reverse.tr('01', '10')
    data = "#{data}0#{b}"
  end
  data[0...length]
end

def calculate_checksum(data)
  checksum = data
  while checksum.length.even?
    new_checksum = ''
    (0...checksum.length).step(2) do |i|
      new_checksum << (checksum[i] == checksum[i+1] ? '1' : '0')
    end
    checksum = new_checksum
  end
  checksum
end

# Read input from file
initial_state = File.read('input.txt').strip

# Generate data and calculate checksum
data = generate_data(initial_state, 272)
checksum = calculate_checksum(data)

# Print the result
puts checksum
