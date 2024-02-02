
total_code_length = 0
total_memory_length = 0
total_encoded_length = 0

File.open("input.txt", "r") do |file|
  file.each_line do |line|
    line = line.chomp
    total_code_length += line.length
    total_memory_length += eval(line).length
    total_encoded_length += line.inspect.length
  end
end

puts total_code_length - total_memory_length
puts total_encoded_length - total_code_length
