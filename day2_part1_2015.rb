total_paper = 0

File.open("input.txt").each do |line|
  l, w, h = line.split('x').map(&:to_i)
  side1 = l * w
  side2 = w * h
  side3 = h * l
  extra = [side1, side2, side3].min
  total_paper += 2*l*w + 2*w*h + 2*h*l + extra
end

puts total_paper