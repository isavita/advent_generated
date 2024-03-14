HASH_TABLE_SIZE = 256

class Step
  attr_accessor :label, :num_box, :operation, :number

  def initialize(step_str)
    @label = step_str.rstrip.match(/[a-zA-Z]+/)[0]
    @num_box = hash_string(@label)
    @operation = step_str[(@label.length)..(@label.length)]
    if @operation == "="
      @number = step_str[(@label.length + 1)..-1].to_i
    end
  end

  def hash_string(str)
    res = 0
    str.each_char { |char| res += char.ord; res *= 17; res %= HASH_TABLE_SIZE }
    res
  end
end

def get_boxes(steps_str)
  boxes = Array.new(HASH_TABLE_SIZE) { [] }

  steps_str.each do |step_str|
    step = Step.new(step_str)
    box_contents = boxes[step.num_box]

    case step.operation
    when "-"
      box_contents.each_with_index do |content, i|
        if content.key?(step.label)
          box_contents.slice!(i)
          break
        end
      end
    when "="
      found = false
      box_contents.each do |content|
        if content.key?(step.label)
          content[step.label] = step.number
          found = true
          break
        end
      end
      box_contents << { step.label => step.number } unless found
    end

    boxes[step.num_box] = box_contents if box_contents.any?
  end

  boxes
end

def to_string_boxes(boxes)
  res = ""

  boxes.each_with_index do |box_contents, i|
    next if box_contents.empty?
    res += "Box #{i} : "
    box_contents.each do |content|
      content.each { |k, v| res += "[#{k} #{v}] " }
    end
    res += "\n"
  end

  res
end

def calculate_power(boxes)
  res = 0

  boxes.each_with_index do |box_contents, i|
    box_contents.each_with_index do |content, j|
      content.each_value { |v| res += (i + 1) * (j + 1) * v }
    end
  end

  res
end

def solve(input)
  steps_str = input[0].split(",")
  boxes = get_boxes(steps_str)
  calculate_power(boxes)
end

def read_file(file_name)
  File.readlines(file_name, chomp: true)
end

input = read_file("input.txt")
puts solve(input)