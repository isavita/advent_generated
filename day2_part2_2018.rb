
lines = File.readlines('input.txt').map(&:chomp)
(0..lines.size-2).each do |i|
  ((i+1)..lines.size-1).each do |j|
    diff = 0
    (0..lines[i].size-1).each do |k|
      if lines[i][k] != lines[j][k]
        diff += 1
        break if diff > 1
      end
    end
    if diff == 1
      common = ''
      (0..lines[i].size-1).each do |k|
        common += lines[i][k] if lines[i][k] == lines[j][k]
      end
      puts common
      exit
    end
  end
end
