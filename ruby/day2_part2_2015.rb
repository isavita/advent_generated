
total_paper = 0
total_ribbon = 0

File.open("input.txt").each do |line|
  l, w, h = line.split('x').map(&:to_i)
  
  side1 = l * w
  side2 = w * h
  side3 = h * l
  
  slack = [side1, side2, side3].min
  
  total_paper += 2*l*w + 2*w*h + 2*h*l + slack
  total_ribbon += [2*l+2*w, 2*w+2*h, 2*h+2*l].min + l*w*h
end

puts total_paper
puts total_ribbon
