
file = File.open("input.txt")
vals = file.gets_to_end.split("\n").map { |x| x.to_i }

prev_sum = vals[0] + vals[1] + vals[2]
count = 0
(3...vals.size).each do |i|
    curr_sum = vals[i-2] + vals[i-1] + vals[i]
    count += 1 if curr_sum > prev_sum
    prev_sum = curr_sum
end

puts count
