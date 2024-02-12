
def read_input(filename : String) : Array(Int32)
  File.read(filename).split(" ").map(&.to_i32)
end

def parse_tree(data : Array(Int32), index : Int32) : Tuple(Int32, Int32)
  child_count = data[index].to_i32
  meta_count = data[index + 1].to_i32
  index += 2

  child_values = [] of Int32
  child_count.times do
    child_value, index = parse_tree(data, index)
    child_values << child_value
  end

  value = 0
  if child_count == 0
    meta_count.times do |i|
      value += data[index + i]
    end
  else
    meta_count.times do |i|
      metadata = data[index + i]
      if metadata <= child_count && metadata > 0
        value += child_values[metadata - 1]
      end
    end
  end
  index += meta_count

  {value, index}
end

numbers = read_input("input.txt")
value, _ = parse_tree(numbers, 0)
puts value
