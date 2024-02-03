
class Network
  attr_accessor :instructions, :nodes

  def initialize(instructions, nodes)
    @instructions = instructions
    @nodes = nodes
  end
end

def parse_input(input)
  instructions = input[0]
  nodes = {}

  input[2..].each do |line|
    head, children = parse_line(line)
    nodes[head] = children
  end

  Network.new(instructions, nodes)
end

def parse_line(line)
  parts = line.split(" = ")
  head = parts[0]
  children_trim = parts[1].gsub(/[()]/, "")
  children_parts = children_trim.split(", ")
  children = [children_parts[0], children_parts[1]]

  [head, children]
end

def gcd(a, b)
  while b != 0
    a, b = b, a % b
  end
  a
end

def lcm(a, b)
  (a * b) / gcd(a, b)
end

def lcm_slice(nums)
  return 0 if nums.empty?

  res = nums[0]
  nums[1..].each do |num|
    res = lcm(res, num)
  end

  res
end

def solve(input)
  network = parse_input(input)
  starts = []

  network.nodes.keys.each do |node|
    if node[-1] == 'A'
      starts << node
    end
  end

  steps = Array.new(starts.length, 0)
  instructions_length = network.instructions.length

  starts.each_with_index do |element, i|
    while element[-1] != 'Z'
      instruction = network.instructions[steps[i] % instructions_length]
      if instruction == 'L'
        element = network.nodes[element][0]
      else
        element = network.nodes[element][1]
      end
      steps[i] += 1
    end
  end

  lcm_slice(steps)
end

def read_file(file_name)
  File.readlines(file_name).map(&:chomp)
end

input = read_file("input.txt")
puts solve(input)
