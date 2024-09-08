def next_row(current_row)
  new_row = ''
  (0...current_row.length).each do |i|
    left = i > 0 ? current_row[i-1] : '.'
    center = current_row[i]
    right = i < current_row.length - 1 ? current_row[i+1] : '.'
    
    new_row << if (left == '^' && center == '^' && right == '.') ||
                  (center == '^' && right == '^' && left == '.') ||
                  (left == '^' && center == '.' && right == '.') ||
                  (right == '^' && center == '.' && left == '.')
                 '^'
               else
                 '.'
               end
  end
  new_row
end

def count_safe_tiles(first_row, total_rows)
  current_row = first_row
  safe_count = current_row.count('.')
  
  (total_rows - 1).times do
    current_row = next_row(current_row)
    safe_count += current_row.count('.')
  end
  
  safe_count
end

# Read input
input = File.read('input.txt').strip

# Part 1
puts "Part 1: #{count_safe_tiles(input, 40)}"

# Part 2
puts "Part 2: #{count_safe_tiles(input, 400000)}"
