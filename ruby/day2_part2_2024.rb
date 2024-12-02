
def is_safe?(report)
  diffs = report.each_cons(2).map { |a, b| b - a }
  return false if diffs.any? { |diff| diff.abs > 3 || diff == 0 }
  diffs.all?(&:positive?) || diffs.all?(&:negative?)
end

def is_safe_with_dampener?(report)
  return true if is_safe?(report)
  report.each_with_index do |_, i|
    modified_report = report.reject.with_index { |_, j| j == i }
    return true if is_safe?(modified_report)
  end
  false
end

def solve(filename, part)
  File.readlines(filename).map(&:split).map { |report| report.map(&:to_i) }.count do |report|
    part == 1 ? is_safe?(report) : is_safe_with_dampener?(report)
  end
end

puts "Part 1: #{solve('input.txt', 1)}"
puts "Part 2: #{solve('input.txt', 2)}"

