class Particle
  attr_reader :index, :p, :v, :a

  def initialize(index, p, v, a)
    @index = index
    @p, @v, @a = [p, v, a].map { |arr| arr.map(&:to_i) }
  end

  def acceleration_magnitude
    @a.map(&:abs).sum
  end

  def velocity_magnitude
    @v.map(&:abs).sum
  end

  def position_magnitude
    @p.map(&:abs).sum
  end
end

particles = File.readlines('input.txt').each_with_index.map do |line, index|
  p, v, a = line.scan(/<(.+?)>/).flatten.map { |s| s.split(',').map(&:to_i) }
  Particle.new(index, p, v, a)
end

closest_particle = particles.min_by do |particle|
  [particle.acceleration_magnitude, particle.velocity_magnitude, particle.position_magnitude]
end

puts closest_particle.index
