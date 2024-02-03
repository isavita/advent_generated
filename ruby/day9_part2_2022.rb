
require 'matrix'

def visited(input, ropelen)
  rope = Array.new(ropelen) { Vector[0, 0] }
  visited = {}
  
  input.each_line do |line|
    b, n = line.split
    d = dir_from_byte(b)
    
    n.to_i.times do
      rope[0] += d
      (1...ropelen).each do |j|
        rope[j] = next_rope(rope[j-1], rope[j])
      end
      visited[rope[ropelen-1]] = true
    end
  end
  
  visited.size
end

def next_rope(head, tail)
  if (head - tail).map(&:abs).max <= 1
    return tail
  end
  return tail + (head - tail).map { |x| x <=> 0 }
end

def dir_from_byte(b)
  from_byte = {
    'N' => Vector[0, 1],
    'E' => Vector[1, 0],
    'S' => Vector[0, -1],
    'W' => Vector[-1, 0],
    'U' => Vector[0, 1],
    'R' => Vector[1, 0],
    'D' => Vector[0, -1],
    'L' => Vector[-1, 0],
    '^' => Vector[0, 1],
    '>' => Vector[1, 0],
    'v' => Vector[0, -1],
    '<' => Vector[-1, 0]
  }
  
  from_byte[b]
end

input = File.read('input.txt')
ropelen = 10
puts visited(input, ropelen)
