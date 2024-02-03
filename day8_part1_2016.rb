input = File.readlines('input.txt').map(&:chomp)

screen = Array.new(6) { Array.new(50, false) }

input.each do |line|
  if line.start_with?("rect")
    _, dimensions = line.split(' ')
    width, height = dimensions.split('x').map(&:to_i)

    (0...height).each do |y|
      (0...width).each do |x|
        screen[y][x] = true
      end
    end
  elsif line.start_with?("rotate row")
    _, _, row, _, amount = line.split(' ')
    row = row.split('=').last.to_i
    amount = amount.to_i

    screen[row] = screen[row].rotate(-amount)
  elsif line.start_with?("rotate column")
    _, _, col, _, amount = line.split(' ')
    col = col.split('=').last.to_i
    amount = amount.to_i

    new_col = Array.new(6, false)
    screen.each_with_index do |row, i|
      new_col[(i + amount) % 6] = row[col]
    end

    screen.each_with_index do |row, i|
      row[col] = new_col[i]
    end
  end
end

lit_pixels = screen.flatten.count(true)
puts lit_pixels