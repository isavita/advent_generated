
def is_safe_report(levels : Array(Int32)) : Bool
  return false if levels.size < 2
  first_diff = levels[1] - levels[0]
  return false if first_diff == 0
  is_increasing = first_diff > 0
  (0...levels.size - 1).each do |i|
    diff = levels[i + 1] - levels[i]
    return false if diff == 0
    return false if (is_increasing && diff <= 0) || (!is_increasing && diff >= 0)
    abs_diff = diff.abs
    return false if abs_diff < 1 || abs_diff > 3
  end
  true
end

def is_safe_with_one_removal(levels : Array(Int32)) : Bool
  (0...levels.size).each do |i|
    modified_levels = levels[0...i] + levels[i + 1..-1]
    return true if is_safe_report(modified_levels)
  end
  false
end

file = File.open("input.txt")
safe_report_count = 0
file.each_line do |line|
  levels = line.split.map(&.to_i)
  safe_report_count += 1 if is_safe_report(levels) || is_safe_with_one_removal(levels)
end
file.close
puts safe_report_count
