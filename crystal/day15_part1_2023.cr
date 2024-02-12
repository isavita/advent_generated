
struct Step
  label : String
  num_box : Int32
  operation : String
  number : Int32
end

def hash_string(str : String) : Int32
  res = 0
  str.each_char do |char|
    res += char.ord
    res *= 17
    res %= 256
  end
  res
end

def parse_step(step_str : String) : Step
  step = Step.new

  step.label = step_str.chomp("=-0123456789")
  step.num_box = hash_string(step.label)
  step.operation = step_str[step.label.size]
  if step.operation == "="
    step.number = step_str[step.label.size + 1..-1].to_i
  end

  step
end

def solve(input : Array(String)) : Int32
  line = input[0]
  steps = line.split(",")
  res = 0
  steps.each do |step|
    res += hash_string(step)
  end
  res
end

def read_file(file_name : String) : Array(String)
  file = File.read(file_name)
  file.split("\n")
end

input = read_file("input.txt")
puts solve(input)
