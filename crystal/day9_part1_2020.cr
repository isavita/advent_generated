
def find_invalid_number(preamble_length : Int32, numbers : Array(Int64)) : Int64?
  (preamble_length...numbers.size).each do |i|
    target = numbers[i]
    preamble = numbers[i - preamble_length, preamble_length]
    valid = false

    preamble.each_with_index do |a, index_a|
      (index_a + 1...preamble.size).each do |index_b|
        valid = true if a + preamble[index_b] == target
        break if valid
      end
      break if valid
    end

    return target unless valid
  end

  nil
end

def main
  input_file = "input.txt"
  numbers = File.read(input_file).split.map(&.to_i64)

  preamble_length = 25
  invalid_number = find_invalid_number(preamble_length, numbers)

  if invalid_number
    puts "The first number that does not have the property is: #{invalid_number}"
  else
    puts "All numbers have the property."
  end
end

main
