struct Component
  getter a : Int32
  getter b : Int32

  def initialize(a : Int32, b : Int32)
    @a = a
    @b = b
  end
end

max_strength = 0
max_length = 0

def find_strongest_longest_bridge(components : Array(Component), used : Array(Bool), port : Int32, strength : Int32, length : Int32, max_strength : Int32, max_length : Int32)
  if length > max_length || (length == max_length && strength > max_strength)
    max_strength = strength
    max_length = length
  end

  components.each_with_index do |c, i|
    next if used[i]

    if c.a == port || c.b == port
      used[i] = true
      next_port = c.a == port ? c.b : c.a
      max_strength, max_length = find_strongest_longest_bridge(components, used, next_port, strength + c.a + c.b, length + 1, max_strength, max_length)
      used[i] = false
    end
  end

  return max_strength, max_length
end

components = [] of Component
File.open("input.txt") do |file|
  file.each_line do |line|
    a, b = line.split("/").map(&.to_i)
    components << Component.new(a, b)
  end
end

used = [false] * components.size
max_strength, _ = find_strongest_longest_bridge(components, used, 0, 0, 0, max_strength, max_length)

puts max_strength