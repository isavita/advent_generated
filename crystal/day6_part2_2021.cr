file = File.open("input.txt")
lantern_fish_counts = Array.new(9, 0_i64) # Use 0_i64 to avoid overflow

file.each_line do |line|
  fish_ages = line.split(',').map(&.to_i)
  fish_ages.each do |age|
    lantern_fish_counts[age] += 1
  end
end

256.times do
  new_lantern_fish = lantern_fish_counts[0]
  8.times do |j|
    lantern_fish_counts[j] = lantern_fish_counts[j + 1]
  end
  lantern_fish_counts[6] += new_lantern_fish
  lantern_fish_counts[8] = new_lantern_fish
end

puts lantern_fish_counts.sum