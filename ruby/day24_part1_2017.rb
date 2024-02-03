
components = File.readlines('input.txt').map(&:chomp).map { |line| line.split('/').map(&:to_i) }

def build_bridge(components, current_port, strength)
  valid_components = components.select { |c| c.include?(current_port) }
  return strength if valid_components.empty?

  strengths = valid_components.map do |component|
    next_port = component[0] == current_port ? component[1] : component[0]
    remaining_components = components - [component]
    build_bridge(remaining_components, next_port, strength + component.sum)
  end

  strengths.max
end

puts build_bridge(components, 0, 0)
