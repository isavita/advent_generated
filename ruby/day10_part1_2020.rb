
adapters = File.readlines('input.txt').map(&:to_i).sort
adapters.unshift(0)
adapters.push(adapters.last + 3)

diff_1 = 0
diff_3 = 0

(adapters.length - 1).times do |i|
  diff = adapters[i + 1] - adapters[i]
  diff_1 += 1 if diff == 1
  diff_3 += 1 if diff == 3
end

puts diff_1 * diff_3
