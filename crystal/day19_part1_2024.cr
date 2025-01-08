
def can_make(design, patterns)
  n = design.size
  dp = Array.new(n + 1, false)
  dp[0] = true
  (1..n).each do |i|
    patterns.each do |p|
      lp = p.size
      if i >= lp && dp[i - lp] && design[i - lp, lp] == p
        dp[i] = true
        break
      end
    end
  end
  dp[n]
end

f = File.open("input.txt")
available_patterns = f.gets.not_nil!.strip.split(",").map(&.strip)
f.gets

count = 0
while line = f.gets
  design = line.strip
  count += 1 if can_make(design, available_patterns)
end
f.close
puts count
