
require 'set'

def build_bridges(components, current_port, current_strength, current_length)
  next_components = components.select { |c| c.include?(current_port) }
  return [current_strength, current_length] if next_components.empty?

  max_strength = current_strength
  max_length = current_length

  next_components.each do |component|
    next_port = component[0] == current_port ? component[1] : component[0]
    next_strength = current_strength + component.sum
    next_length = current_length + 1
    next_components = components - [component]

    strength, length = build_bridges(next_components, next_port, next_strength, next_length)

    if length > max_length || (length == max_length && strength > max_strength)
      max_strength = strength
      max_length = length
    end
  end

  [max_strength, max_length]
end

components = File.readlines('input.txt').map { |line| line.split('/').map(&:to_i) }

strength, _ = build_bridges(components, 0, 0, 0)
puts strength
