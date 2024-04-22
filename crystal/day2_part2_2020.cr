file = File.new("input.txt", "r")
valid_count = 0

file.each_line do |line|
  policy, password = line.split(": ")
  min, max, char = policy.split(" ")[0].split("-").map(&.to_i) + [policy.split(" ")[1][0]]
  valid_count += 1 if (password[min - 1] == char) != (password[max - 1] == char)
end

puts valid_count