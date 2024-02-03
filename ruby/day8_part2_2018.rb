
def sum_metadata(input)
  data = input.split.map(&:to_i)
  sum_metadata_helper(data)
end

def sum_metadata_helper(data)
  num_child_nodes = data.shift
  num_metadata = data.shift
  sum = 0

  num_child_nodes.times do
    sum += sum_metadata_helper(data)
  end

  num_metadata.times do
    sum += data.shift
  end

  sum
end

def calculate_node_value(input)
  data = input.split.map(&:to_i)
  calculate_node_value_helper(data)
end

def calculate_node_value_helper(data)
  num_child_nodes = data.shift
  num_metadata = data.shift
  child_values = []

  num_child_nodes.times do
    child_values << calculate_node_value_helper(data)
  end

  metadata_values = num_metadata.times.map { data.shift }

  if num_child_nodes == 0
    metadata_values.sum
  else
    metadata_values.map do |metadata|
      if metadata == 0 || metadata > num_child_nodes
        0
      else
        child_values[metadata - 1]
      end
    end.sum
  end
end

input = File.read('input.txt').chomp

puts sum_metadata(input)
puts calculate_node_value(input)
