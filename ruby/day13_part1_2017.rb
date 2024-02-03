
firewall = File.readlines('input.txt').map { |line| line.split(': ').map(&:to_i) }

severity = firewall.reduce(0) do |sum, layer|
  depth, range = layer
  sum + (depth % ((range - 1) * 2) == 0 ? depth * range : 0)
end

puts severity
