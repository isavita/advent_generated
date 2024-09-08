class Particle
  attr_accessor :p, :v, :a

  def initialize(p, v, a)
    @p, @v, @a = p, v, a
  end

  def update
    3.times do |i|
      @v[i] += @a[i]
      @p[i] += @v[i]
    end
  end

  def position_key
    @p.join(',')
  end
end

def parse_input(file_path)
  File.readlines(file_path).map do |line|
    p, v, a = line.scan(/<(.+?)>/).map { |coords| coords[0].split(',').map(&:to_i) }
    Particle.new(p, v, a)
  end
end

def simulate(particles, ticks)
  ticks.times do
    positions = Hash.new { |h, k| h[k] = [] }

    particles.each do |particle|
      particle.update
      positions[particle.position_key] << particle
    end

    particles = positions.values.reject { |collided| collided.size > 1 }.flatten
  end

  particles.size
end

particles = parse_input('input.txt')
result = simulate(particles, 1000)
puts result
