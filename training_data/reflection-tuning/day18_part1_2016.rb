def generate_next_row(current_row)
  next_row = ''
  (0...current_row.length).each do |i|
    left = i > 0 ? current_row[i-1] : '.'
    center = current_row[i]
    right = i < current_row.length - 1 ? current_row[i+1] : '.'
    
    next_row << if (left == '^' && center == '^' && right == '.') ||
                   (center == '^' && right == '^' && left == '.') ||
                   (left == '^' && center == '.' && right == '.') ||
                   (right == '^' && center == '.' && left == '.')
                  '^'
                else
                  '.'
                end
  end
  next_row
end

# Read the initial row from the input file
initial_row = File.read('input.txt').strip

# Initialize variables
current_row = initial_row
total_safe_tiles = current_row.count('.')
rows_to_generate = 39  # We already have the initial row, so we need 39 more

# Generate the remaining rows and count safe tiles
rows_to_generate.times do
  current_row = generate_next_row(current_row)
  total_safe_tiles += current_row.count('.')
end

# Print the final answer
puts total_safe_tiles
