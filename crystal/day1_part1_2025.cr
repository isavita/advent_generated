
# Crystal version of the given solution
# Reads instructions from "input.txt" and prints the number of times
# the position becomes zero (the password).

pos = 50
zeros = 0

File.each_line("input.txt") do |raw|
  line = raw.strip
  next if line.empty?

  dir = line[0]
  amount = line[1..].to_i

  pos = case dir
        when 'R' then pos + amount
        when 'L' then pos - amount
        else          pos
        end
  pos %= 100
  pos += 100 if pos < 0
  zeros += 1 if pos == 0
end

puts "The password is: #{zeros}"
