
width, height = 101, 103
robots = []

File.foreach("input.txt") do |line|
  parts = line.split(" ")
  p_part = parts[0].sub("p=", "")
  v_part = parts[1].sub("v=", "")
  pos = p_part.split(",").map(&:to_i)
  vel = v_part.split(",").map(&:to_i)
  robots << [pos[0], pos[1], vel[0], vel[1]]
end

100.times do
  robots.each do |r|
    r[0] = (r[0] + r[2]) % width
    r[0] += width if r[0] < 0
    r[1] = (r[1] + r[3]) % height
    r[1] += height if r[1] < 0
  end
end

q1, q2, q3, q4 = 0, 0, 0, 0
robots.each do |r|
  x, y = r[0], r[1]
  next if x == 50 || y == 51
  if x < 50 && y < 51
    q1 += 1
  elsif x > 50 && y < 51
    q2 += 1
  elsif x < 50 && y > 51
    q3 += 1
  elsif x > 50 && y > 51
    q4 += 1
  end
end

puts q1 * q2 * q3 * q4
