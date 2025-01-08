
WIDTH = 101
HEIGHT = 103

robots = [] of Array(Int32)
File.open("input.txt") do |file|
  file.each_line do |line|
    parts = line.split(" ")
    pos = parts[0][2..].split(",").map(&.to_i)
    vel = parts[1][2..].split(",").map(&.to_i)
    robots << [pos[0], pos[1], vel[0], vel[1]]
  end
end

100.times do
  robots.each do |r|
    r[0] = (r[0] + r[2]) % WIDTH
    r[1] = (r[1] + r[3]) % HEIGHT
    r[0] += WIDTH if r[0] < 0
    r[1] += HEIGHT if r[1] < 0
  end
end

q1 = q2 = q3 = q4 = 0
robots.each do |r|
  x, y = r[0], r[1]
  next if x == 50 || y == 51
  q1 += 1 if x < 50 && y < 51
  q2 += 1 if x > 50 && y < 51
  q3 += 1 if x < 50 && y > 51
  q4 += 1 if x > 50 && y > 51
end

puts q1 * q2 * q3 * q4
