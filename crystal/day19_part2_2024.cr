
#!/usr/bin/env crystal

def count_ways(design : String, patterns : Array(String), lengths : Array(Int32)) : Int64
  n = design.bytesize
  dp = Array(Int64).new(n + 1, 0)
  dp[0] = 1

  (1..n).each do |i|
    patterns.each_with_index do |pat, idx|
      lp = lengths[idx]
      next if lp > i
      if design[i - lp, lp] == pat
        dp[i] += dp[i - lp]
      end
    end
  end

  dp[n]
end

total_ways = 0_i64

File.open("input.txt", "r") do |file|
  patterns_line = file.gets
  unless patterns_line
    puts 0
    exit
  end

  patterns = patterns_line.chomp.split(',').map { |t| t.strip }.reject { |t| t.empty? }
  lengths = patterns.map(&.bytesize)

  # discard separator line
  file.gets

  file.each_line do |line|
    design = line.strip
    next if design.empty?
    total_ways += count_ways(design, patterns, lengths)
  end
end

puts total_ways
