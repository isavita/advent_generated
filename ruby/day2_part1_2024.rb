def is_safe?(report)
  diffs = report.each_cons(2).map { |a, b| b - a }
  return false if diffs.any? { |diff| diff.abs > 3 || diff == 0 }
  diffs.all?(&:positive?) || diffs.all?(&:negative?)
end

def solve(filename, part)
  File.readlines(filename).map(&:split).map { |report| report.map(&:to_i) }.count do |report|
    is_safe?(report)
  end
end

puts solve('input.txt', 1)

