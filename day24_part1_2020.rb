
input = File.read('input.txt').split("\n")

tiles = Hash.new(0)

input.each do |line|
  x, y = 0, 0
  i = 0
  while i < line.length
    case line[i]
    when 'e'
      x += 1
      i += 1
    when 'w'
      x -= 1
      i += 1
    when 's'
      if line[i+1] == 'e'
        x += 1
        y -= 1
      else
        y -= 1
      end
      i += 2
    when 'n'
      if line[i+1] == 'w'
        x -= 1
        y += 1
      else
        y += 1
      end
      i += 2
    end
  end

  tiles[[x, y]] += 1
end

black_tiles = tiles.select { |k, v| v.odd? }.count

puts black_tiles
