
class Component
  property a : Int32
  property b : Int32

  def initialize(a : Int32, b : Int32)
    @a = a
    @b = b
  end
end

max_strength = 0

def find_strongest_bridge(components : Array(Component), used : Array(Bool), port : Int32, strength : Int32, max_strength : Int32)
  max_strength = [max_strength, strength].max

  components.each_with_index do |c, i|
    next if used[i]

    if c.a == port || c.b == port
      used[i] = true
      next_port = c.a == port ? c.b : c.a
      max_strength = find_strongest_bridge(components, used, next_port, strength + c.a + c.b, max_strength)
      used[i] = false
    end
  end

  max_strength
end

components = [] of Component
File.open("input.txt") do |file|
  file.each_line do |line|
    a, b = line.split("/").map(&.to_i)
    components << Component.new(a, b)
  end
end

used = Array(Bool).new(components.size, false)
max_strength = find_strongest_bridge(components, used, 0, 0, max_strength)

puts max_strength
