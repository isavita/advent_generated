
def simulate_lanternfish(days)
  fish_timers = File.read("input.txt").split(",").map(&:to_i)
  fish_counts = Array.new(9, 0)
  fish_timers.each { |timer| fish_counts[timer] += 1 }
  days.times do
    fish_counts.rotate!
    fish_counts[6] += fish_counts[8]
  end
  fish_counts.sum
end

puts simulate_lanternfish(80)
puts simulate_lanternfish(256)
