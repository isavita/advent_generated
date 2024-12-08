
def can_produce(target, nums, idx, current)
  return current == target if idx == nums.length
  can_produce(target, nums, idx + 1, current + nums[idx]) || can_produce(target, nums, idx + 1, current * nums[idx])
end

total = 0
File.open("input.txt").each_line do |line|
  next if line.strip.empty?
  parts = line.split(":")
  target = parts[0].to_i
  nums = parts[1].split.map(&:to_i)
  if nums.length == 1
    total += target if nums[0] == target
    next
  end
  total += target if can_produce(target, nums, 1, nums[0])
end
puts total
