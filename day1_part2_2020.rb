expenses = File.readlines('input.txt').map(&:to_i)

expenses.combination(2).each do |a, b|
  if a + b == 2020
    puts a * b
    break
  end
end

expenses.combination(3).each do |a, b, c|
  if a + b + c == 2020
    puts a * b * c
    break
  end
end