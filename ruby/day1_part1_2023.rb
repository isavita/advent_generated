
sum = 0
File.open("input.txt", "r") do |file|
  file.each_line do |line|
    digits = line.scan(/\d/)
    sum += (digits[0] + digits[-1]).to_i
  end
end

puts sum
