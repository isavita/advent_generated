
total = File.readlines('input.txt').sum do |line|
  l, w, h = line.split('x').map(&:to_i)
  sides = [l*w, w*h, h*l]
  sides.sum * 2 + sides.min
end

puts total
