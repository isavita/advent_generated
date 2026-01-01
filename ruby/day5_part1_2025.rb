
def main
  lines = File.readlines('input.txt', chomp: true)
  i = 0
  ranges = []
  while i < lines.size && !lines[i].empty?
    a, b = lines[i].split('-').map(&:to_i)
    ranges << [a, b]
    i += 1
  end

  unless ranges.empty?
    ranges.sort_by! { |r| r[0] }
    merged = []
    ranges.each do |r|
      if merged.empty? || r[0] > merged[-1][1]
        merged << r
      elsif r[1] > merged[-1][1]
        merged[-1][1] = r[1]
      end
    end
    ranges = merged
  end

  i += 1 # skip blank line
  count = 0
  while i < lines.size
    s = lines[i]
    unless s.empty?
      id = s.to_i
      l = 0
      r = ranges.size
      while l < r
        m = (l + r) >> 1
        if id < ranges[m][0]
          r = m
        elsif id > ranges[m][1]
          l = m + 1
        else
          count += 1
          break
        end
      end
    end
    i += 1
  end

  puts "Number of fresh ingredients: #{count}"
end

main
