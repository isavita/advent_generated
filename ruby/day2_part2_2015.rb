
total_ribbon = File.readlines('input.txt').sum do |line|
  l, w, h = line.split('x').map(&:to_i).sort
  l * w * h + 2 * (l + w)
end

puts total_ribbon
