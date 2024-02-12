
data = File.read("input.txt").chomp.split("\n")
checksum = 0

data.each do |line|
  nums = line.split("\t") # Split using tabs
  min_val = nums[0].to_i
  max_val = nums[0].to_i

  nums.each do |num_str|
    num = num_str.to_i
    min_val = num if num < min_val
    max_val = num if num > max_val
  end

  checksum += (max_val - min_val)
end

puts checksum
