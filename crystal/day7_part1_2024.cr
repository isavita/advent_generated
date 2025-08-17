
#!/usr/bin/env crystal

def evaluate_expression(target : Int64, numbers : Array(Int32)) : Bool
  count = numbers.size
  return false if count == 0
  return numbers[0].to_i64 == target if count == 1

  ops = count - 1
  limit = 1 << ops
  limit.times do |i|
    result = numbers[0].to_i64
    mask = i
    j = 0
    while j < ops
      if (mask & 1) == 0
        result += numbers[j + 1].to_i64
      else
        result *= numbers[j + 1].to_i64
      end
      mask >>= 1
      j += 1
    end
    return true if result == target
  end
  false
end

total = 0_i64

File.open("input.txt") do |file|
  file.each_line do |line|
    line = line.strip
    next if line.empty?
    parts = line.split(':', 2)
    next unless parts.size == 2
    target = parts[0].to_i64
    nums = parts[1].strip.split.map(&.to_i32)
    next if nums.empty?
    total += target if evaluate_expression(target, nums)
  end
end

puts total
