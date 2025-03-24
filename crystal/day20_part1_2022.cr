
def mix(numbers : Array(Int32))
  mixed = numbers.dup
  indices = (0...numbers.size).to_a

  numbers.each_with_index do |number, original_index|
    current_index = indices.index(original_index)

    if current_index.nil?
        raise "Original index not found. This should not happen."
    end
    
    new_index = (current_index + number) % (numbers.size - 1)
    new_index += (numbers.size - 1) if new_index < 0  # Ensure positive index

    # Move the element
    mixed.delete_at(current_index)
    mixed.insert(new_index, number)
        
    # Update the index tracking.  Crucial optimization
    indices.delete_at(current_index)
    indices.insert(new_index, original_index)

  end
  mixed
end

def grove_coordinates_sum(mixed : Array(Int32))
  zero_index = mixed.index(0)
  raise "Zero not found in the mixed array." if zero_index.nil?

  size = mixed.size
  sum = 0_i64
  sum += mixed[(zero_index + 1000) % size]
  sum += mixed[(zero_index + 2000) % size]
  sum += mixed[(zero_index + 3000) % size]
  sum
end

def main
  begin
    numbers = [] of Int32
    File.open("./input.txt") do |file|
      file.each_line do |line|
        numbers << line.strip.to_i32
      end
    end

    mixed_numbers = mix(numbers)
    result = grove_coordinates_sum(mixed_numbers)
    puts result

  rescue ex : File::NotFoundError
    puts "Error: input.txt not found."
  rescue ex
    puts "An error occurred: #{ex.message}"
  end
end

main
