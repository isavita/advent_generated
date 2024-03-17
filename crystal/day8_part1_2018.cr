def read_input(filename : String) : Array(Int32)
  numbers = [] of Int32
  File.open(filename) do |file|
    file.gets.not_nil!.split(" ").each { |part| numbers << part.to_i }
  end
  numbers
end

def parse_tree(data : Array(Int32), index : Int32) : Tuple(Int32, Int32)
  child_count, meta_count = data[index], data[index + 1]
  index += 2

  sum = 0
  child_count.times do
    child_sum, new_index = parse_tree(data, index)
    sum += child_sum
    index = new_index
  end

  meta_count.times do
    sum += data[index]
    index += 1
  end

  {sum, index}
end

numbers = read_input("input.txt")
sum, _ = parse_tree(numbers, 0)
puts sum