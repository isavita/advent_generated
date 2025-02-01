
def can_make(design, patterns)
  n = design.length
  dp = [false] * (n + 1)
  dp[0] = true
  (1..n).each do |i|
    patterns.each do |p|
      lp = p.length
      dp[i] = true if i >= lp && dp[i - lp] && design[i - lp...i] == p
      break if dp[i]
    end
  end
  dp[n]
end

file = File.open('input.txt')
available_patterns = file.readline.strip.split(',').map(&:strip)
file.readline
count = 0
file.each_line do |line|
  design = line.strip
  count += 1 if can_make(design, available_patterns)
end
file.close
puts count
