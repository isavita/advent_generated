
total_code = 0
total_memory = 0

File.open("input.txt", "r") do |file|
  file.each_line do |line|
    line = line.chomp
    total_code += line.length
    total_memory += eval(line).length
  end
end

puts total_code - total_memory
