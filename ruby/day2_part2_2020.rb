
input = File.readlines('input.txt')

valid_count = 0
input.each do |line|
  policy, password = line.split(':').map(&:strip)
  range, letter = policy.split(' ')
  min, max = range.split('-').map(&:to_i)

  # Part One
  # count = password.count(letter)
  # valid_count += 1 if count >= min && count <= max

  # Part Two
  valid_count += 1 if (password[min - 1] == letter) ^ (password[max - 1] == letter)
end

puts valid_count
