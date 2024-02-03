valid_passwords = 0

File.open("input.txt").each do |line|
  policy, password = line.split(":").map(&:strip)
  range, letter = policy.split(" ")
  min, max = range.split("-").map(&:to_i)

  count = password.count(letter)
  valid_passwords += 1 if count >= min && count <= max
end

puts valid_passwords