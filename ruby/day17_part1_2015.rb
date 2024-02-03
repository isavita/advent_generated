input = File.readlines('input.txt').map(&:to_i)
count = 0

(1..input.length).each do |n|
  input.combination(n).each do |combo|
    count += 1 if combo.sum == 150
  end
end

puts count