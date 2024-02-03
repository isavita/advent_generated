
data = File.read('input.txt').lines.map { |line| line.scan(/-?\d+/).map(&:to_i) }

particles = data.map.with_index do |(px, py, pz, vx, vy, vz, ax, ay, az), i|
  { index: i, pos: [px, py, pz], vel: [vx, vy, vz], acc: [ax, ay, az] }
end

def manhattan_distance(pos)
  pos.map(&:abs).sum
end

def update_particle(particle)
  3.times do |i|
    particle[:vel][i] += particle[:acc][i]
    particle[:pos][i] += particle[:vel][i]
  end
end

closest_particle = particles.min_by do |particle|
  acc_distance = manhattan_distance(particle[:acc])
  vel_distance = manhattan_distance(particle[:vel])
  pos_distance = manhattan_distance(particle[:pos])
  [acc_distance, vel_distance, pos_distance, particle[:index]]
end

puts closest_particle[:index]
