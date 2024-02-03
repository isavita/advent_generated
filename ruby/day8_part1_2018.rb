
input = File.read('input.txt').split.map(&:to_i)

def sum_metadata(input)
  child_nodes = input.shift
  metadata_entries = input.shift
  sum = 0

  child_nodes.times do
    sum += sum_metadata(input)
  end

  metadata_entries.times do
    sum += input.shift
  end

  sum
end

puts sum_metadata(input)
