
def concat(a, b)
  (a.to_s + b.to_s).to_i
end

def can_produce(target, nums, idx, value)
  return value == target if idx == nums.length
  n = nums[idx]
  return true if can_produce(target, nums, idx + 1, value + n)
  return true if can_produce(target, nums, idx + 1, value * n)
  return true if can_produce(target, nums, idx + 1, concat(value, n))
  false
end

total = 0
File.open("input.txt").each_line do |line|
  next if line.strip.empty?
  parts = line.split(":")
  target = parts[0].to_i
  nums = parts[1].split.map(&:to_i)
  next if nums.empty?

  if nums.length == 1
    total += target if nums[0] == target
    next
  end

  total += target if can_produce(target, nums, 1, nums[0])
end

puts total
