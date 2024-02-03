
def parse_regex(input)
  stack, graph = [], Hash.new { |h, k| h[k] = { x: 0, y: 0, doors: 0 } }
  x, y, doors = 0, 0, 0
  input.each_char do |char|
    case char
    when 'N', 'S', 'E', 'W'
      case char
      when 'N' then y -= 1
      when 'S' then y += 1
      when 'E' then x += 1
      when 'W' then x -= 1
      end
      doors += 1
      graph[[x, y]][:x] = x
      graph[[x, y]][:y] = y
      graph[[x, y]][:doors] = [graph[[x, y]][:doors], doors].max
    when '('
      stack.push([x, y, doors])
    when '|'
      x, y, doors = stack.last
    when ')'
      x, y, doors = stack.pop
    end
  end
  graph
end

def max_doors(graph)
  graph.values.map { |v| v[:doors] }.max
end

def rooms_with_min_doors(graph, min_doors)
  graph.count { |_, v| v[:doors] >= min_doors }
end

input = File.read("input.txt").strip[1..-2] # Remove ^ and $
graph = parse_regex(input)

puts "Part 1: #{max_doors(graph)}"
puts "Part 2: #{rooms_with_min_doors(graph, 1000)}"
