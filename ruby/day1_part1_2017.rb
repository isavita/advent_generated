
puts File.read("input.txt").strip.chars.map(&:to_i).each_with_index.inject(0) { |sum, (num, index)| num == File.read("input.txt").strip.chars[(index + 1) % File.read("input.txt").strip.length].to_i ? sum + num : sum }
