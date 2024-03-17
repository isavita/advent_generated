File.open("input.txt", "r") do |file|
  valid_passwords = 0
  file.each_line do |line|
    policy, password = line.chomp.split(": ")
    range, letter = policy.split(" ")
    min, max = range.split("-").map(&.to_i)
    count = password.count(letter)
    valid_passwords += 1 if min <= count <= max
  end
  puts valid_passwords
end