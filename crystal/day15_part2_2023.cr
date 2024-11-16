
class Step
  property label : String
  property num_box : Int32
  property operation : String
  property number : Int32

  def initialize(@label : String, @num_box : Int32, @operation : String, @number : Int32 = 0)
  end
end

def hash_string(str : String) : Int32
  res = 0
  str.each_byte do |char|
    res += char
    res *= 17
    res %= 256
  end
  res
end

def parse_step(step_str : String) : Step
  label = step_str.gsub(/[=-]\d*$/, "")
  num_box = hash_string(label)
  operation = step_str[label.size, 1]
  number = operation == "=" ? step_str[label.size + 1..-1].to_i : 0

  Step.new(label, num_box, operation, number)
end

def get_boxes(steps_str : Array(String)) : Hash(Int32, Array(Hash(String, Int32)))
  boxes = {} of Int32 => Array(Hash(String, Int32))

  steps_str.each do |step_str|
    step = parse_step(step_str)
    box_contents = boxes[step.num_box]? || [] of Hash(String, Int32)

    case step.operation
    when "-"
      box_contents.reject! { |content| content.has_key?(step.label) }
    when "="
      found = false
      box_contents.each do |content|
        if content.has_key?(step.label)
          content[step.label] = step.number
          found = true
          break
        end
      end
      box_contents << {step.label => step.number} unless found
    end

    if box_contents.empty?
      boxes.delete(step.num_box)
    else
      boxes[step.num_box] = box_contents
    end
  end

  boxes
end

def calculate_power(boxes : Hash(Int32, Array(Hash(String, Int32)))) : Int32
  power = 0
  boxes.each do |box_num, contents|
    contents.each_with_index do |content, slot_index|
      content.each_value do |value|
        power += (box_num + 1) * (slot_index + 1) * value
      end
    end
  end
  power
end

def solve(input : Array(String)) : Int32
  steps_str = input[0].split(",")
  boxes = get_boxes(steps_str)
  calculate_power(boxes)
end

def read_file(file_name : String) : Array(String)
  File.read_lines(file_name)
end

input = read_file("input.txt")
puts solve(input)
