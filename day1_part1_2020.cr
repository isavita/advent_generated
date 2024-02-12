
numbers = File.read("input.txt").lines.map { |line| line.to_i }

(0...numbers.size - 1).each do |i|
  ((i + 1)...numbers.size).each do |j|
    if numbers[i] + numbers[j] == 2020
      puts numbers[i] * numbers[j]
      break
    end
  end
end
