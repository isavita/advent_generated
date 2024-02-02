
moons = File.readlines('input.txt').map { |line| line.scan(/-?\d+/).map(&:to_i) }
velocities = Array.new(4) { [0, 0, 0] }

1000.times do
  moons.each_with_index do |moon, i|
    moons.each_with_index do |other_moon, j|
      next if i == j

      3.times do |axis|
        velocities[i][axis] += other_moon[axis] <=> moon[axis]
      end
    end
  end

  moons.each_with_index do |moon, i|
    3.times do |axis|
      moon[axis] += velocities[i][axis]
    end
  end
end

total_energy = 0

moons.each_with_index do |moon, i|
  potential_energy = moon.map(&:abs).sum
  kinetic_energy = velocities[i].map(&:abs).sum
  total_energy += potential_energy * kinetic_energy
end

puts total_energy
