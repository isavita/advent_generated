class Particle
  property p : Array(Int32)
  property v : Array(Int32)
  property a : Array(Int32)

  def initialize
    @p = Array(Int32).new(3, 0)
    @v = Array(Int32).new(3, 0)
    @a = Array(Int32).new(3, 0)
  end
end

particles = [] of Particle

File.each_line("input.txt") do |line|
  parts = line.split(", ")
  particle = Particle.new
  parts.each_with_index do |part, i|
    coords = part[3..-2].split(",")
    coords.each_with_index do |coord, j|
      case i
      when 0
        particle.p[j] = coord.to_i
      when 1
        particle.v[j] = coord.to_i
      when 2
        particle.a[j] = coord.to_i
      end
    end
  end
  particles << particle
end

1000.times do
  positions = Hash(String, Int32).new(0)
  particles.each do |particle|
    3.times do |j|
      particle.v[j] += particle.a[j]
      particle.p[j] += particle.v[j]
    end
    pos_str = "#{particle.p[0]},#{particle.p[1]},#{particle.p[2]}"
    positions[pos_str] += 1
  end

  particles = particles.select do |particle|
    pos_str = "#{particle.p[0]},#{particle.p[1]},#{particle.p[2]}"
    positions[pos_str] == 1
  end
end

puts particles.size