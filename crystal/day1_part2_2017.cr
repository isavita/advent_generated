File.open("input.txt", "r") do |file|
  input = file.gets.not_nil!.chomp
  halfway = input.size // 2
  sum = 0

  (0...input.size).each do |i|
    next_index = (i + halfway) % input.size
    sum += input[i].to_i if input[i] == input[next_index]
  end

  puts sum
end