
def gcd(a, b)
  while b != 0
    a, b = b, a % b
  end
  a
end

def lcm(a, b)
  (a * b) // gcd(a, b)
end

def lcm_slice(nums)
  return 0 if nums.empty?
  res = nums[0]
  nums.each_with_index do |num, i|
    next if i == 0
    res = lcm(res, num)
  end
  res
end

def solve(input)
  instructions = input[0]
  nodes = {} of String => Tuple(String, String)
  input[2..].each do |line|
    head, children_str = line.split(" = ")
    children = children_str[1..-2].split(", ")
    nodes[head] = {children[0], children[1]}
  end

  starts = nodes.keys.select { |node| node.ends_with?('A') }
  steps = [] of Int64
  instructions_length = instructions.size

  starts.each do |start|
    current = start
    count = 0_i64
    until current.ends_with?('Z')
      instruction = instructions[count % instructions_length]
      current = instruction == 'L' ? nodes[current][0] : nodes[current][1]
      count += 1
    end
    steps << count
  end

  lcm_slice(steps)
end

input = File.read("input.txt").split("\n")
puts solve(input)
