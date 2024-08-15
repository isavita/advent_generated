invalid_number = 14360655

numbers = File.read_lines("input.txt").map(&.to_i64)

numbers.each_with_index do |n, i|
  sum = n
  min = n
  max = n
  (i + 1).upto(numbers.size - 1) do |j|
    sum += numbers[j]
    min = [min, numbers[j]].min
    max = [max, numbers[j]].max
    if sum == invalid_number
      puts min + max
      exit
    elsif sum > invalid_number
      break
    end
  end
end