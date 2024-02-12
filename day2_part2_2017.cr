
data = File.read("input.txt").chomp
lines = data.split("\n")
sum = 0

lines.each do |line|
  nums = line.split("\t").map { |num| num.to_i }

  nums.each_with_index do |num1, i|
    nums.each_with_index do |num2, j|
      if i != j && num1 % num2 == 0
        sum += num1 / num2
      end
    end
  end
end

puts sum
