
def parse_input(file)
  File.read(file).each_line.map do |line|
    line.scan(/-?\d+/).map(&:to_i).each_slice(3).to_a
  end
end

def manhattan_distance(p)
  p[0].abs + p[1].abs + p[2].abs
end

def update_particle(particle)
  3.times { |i| particle[1][i] += particle[2][i] }
  3.times { |i| particle[0][i] += particle[1][i] }
end

def closest_particle(particles)
  particles.each { |particle| update_particle(particle) }
  distances = particles.map { |p| manhattan_distance(p[0]) }
  distances.each_with_index.min[1]
end

def resolve_collisions(particles)
  100.times do
    positions = Hash.new(0)
    particles.each { |particle| update_particle(particle) }
    particles.each { |particle| positions[particle[0]] += 1 }
    particles.reject! { |particle| positions[particle[0]] > 1 }
  end
  particles.count
end

particles = parse_input('input.txt')
puts closest_particle(particles.dup)
puts resolve_collisions(particles)
