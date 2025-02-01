
file = File.open('input.txt')
available_patterns = file.readline.strip.split(',').map(&:strip)
file.readline
total_ways = 0
while (design = file.gets)
  design.strip!
  n = design.length
  dp = Array.new(n + 1, 0)
  dp[0] = 1
  (1..n).each do |i|
    available_patterns.each do |p|
      lp = p.length
      dp[i] += dp[i - lp] if i >= lp && design[i - lp..i - 1] == p
    end
  end
  total_ways += dp[n]
end
file.close
puts total_ways
