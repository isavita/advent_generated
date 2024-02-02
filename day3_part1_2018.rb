claims = File.readlines('input.txt').map(&:chomp)

fabric = Array.new(1000) { Array.new(1000, 0) }

claims.each do |claim|
  parts = claim.split(' ')
  id = parts[0][1..-1].to_i
  left, top = parts[2].split(',').map(&:to_i)
  width, height = parts[3].split('x').map(&:to_i)

  (left...(left + width)).each do |x|
    (top...(top + height)).each do |y|
      fabric[x][y] += 1
    end
  end
end

overlap = 0
fabric.each do |row|
  row.each do |cell|
    overlap += 1 if cell > 1
  end
end

puts overlap