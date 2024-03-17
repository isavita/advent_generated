def abs(x)
  x < 0 ? -x : x
end

def max(a, b)
  a > b ? a : b
end

def distance(x, y, z)
  (abs(x) + abs(y) + abs(z)) // 2
end

File.open("input.txt", "r") do |file|
  input = file.gets.try(&.chomp)
  if input.nil?
    puts "Error: No input found in the file."
    exit 1
  end

  directions = input.split(",")

  x = y = z = 0
  max_distance = 0

  directions.each do |dir|
    case dir
    when "n"
      y += 1
      z -= 1
    when "ne"
      x += 1
      z -= 1
    when "se"
      x += 1
      y -= 1
    when "s"
      y -= 1
      z += 1
    when "sw"
      x -= 1
      z += 1
    when "nw"
      x -= 1
      y += 1
    end

    cur_distance = distance(x, y, z)
    max_distance = max(max_distance, cur_distance)
  end

  puts distance(x, y, z)
end