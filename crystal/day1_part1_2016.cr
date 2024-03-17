File.open("input.txt", "r") do |file|
  directions = file.gets.try(&.chomp).try(&.split(", "))
  if directions
    x, y = 0, 0
    direction = 0
    directions.each do |dir|
      case dir[0]
      when 'L'
        direction = (direction - 1) % 4
      when 'R'
        direction = (direction + 1) % 4
      end
      case direction
      when 0
        y += dir[1..-1].to_i
      when 1
        x += dir[1..-1].to_i
      when 2
        y -= dir[1..-1].to_i
      when 3
        x -= dir[1..-1].to_i
      end
    end
    puts (x.abs + y.abs)
  else
    puts "Error: No input file or empty file"
  end
end