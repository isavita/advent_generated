
lines = File.read("input.txt").lines
(0...lines.size - 1).each do |i|
  ((i + 1)...lines.size).each do |j|
    diff = 0
    common = ""
    lines[i].size.times do |k|
      if lines[i][k] != lines[j][k]
        diff += 1
        break if diff > 1
      else
        common += lines[i][k]
      end
    end
    if diff == 1
      puts common
      exit
    end
  end
end
