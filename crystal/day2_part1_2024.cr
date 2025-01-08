
def solve
  safe_report_count = 0
  File.each_line("input.txt") do |line|
    levels = parse_levels(line)
    safe_report_count += 1 if is_safe_report(levels)
  end
  puts safe_report_count
end

def parse_levels(line)
  line.split.map(&.to_i)
end

def is_safe_report(levels)
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

solve
