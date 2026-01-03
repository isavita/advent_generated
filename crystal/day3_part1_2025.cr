
def calc(s : String) : Int32
  len = s.bytesize
  9.downto(0) do |d1|
    idx = s.index((d1 + 48).chr)
    next unless idx && idx < len - 1
    max2 = -1
    (idx + 1...len).each do |i|
      c = s[i]
      if c >= '0' && c <= '9'
        v = c.ord - 48
        max2 = v if v > max2
        break if max2 == 9
      end
    end
    return d1 * 10 + max2 if max2 != -1
  end
  0
end

total = 0
File.each_line("input.txt") do |line|
  total += calc(line.chomp)
end

puts total
