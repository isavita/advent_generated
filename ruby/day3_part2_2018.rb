
claims = File.readlines('input.txt').map(&:chomp)

fabric = Hash.new(0)
overlap = Hash.new(0)

claims.each do |claim|
  id, _, pos, size = claim.split(' ')
  x, y = pos.split(',').map(&:to_i)
  width, height = size.split('x').map(&:to_i)

  (x...x+width).each do |i|
    (y...y+height).each do |j|
      fabric[[i, j]] += 1
      overlap[[i, j]] += 1 if fabric[[i, j]] == 2
    end
  end
end

puts overlap.values.sum

claims.each do |claim|
  id, _, pos, size = claim.split(' ')
  x, y = pos.split(',').map(&:to_i)
  width, height = size.split('x').map(&:to_i)

  intact = true
  (x...x+width).each do |i|
    (y...y+height).each do |j|
      intact = false if fabric[[i, j]] > 1
    end
  end

  puts id if intact
end
