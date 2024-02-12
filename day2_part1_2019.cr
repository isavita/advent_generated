
input_data = File.read("input.txt").split(",").map { |x| x.to_i }

input_data[1] = 12
input_data[2] = 2

def execute_program(data)
  data.each_slice(4) do |slice|
    opcode, pos1, pos2, pos3 = slice
    case opcode
    when 1
      data[pos3] = data[pos1] + data[pos2]
    when 2
      data[pos3] = data[pos1] * data[pos2]
    when 99
      return data[0]
    else
      raise "Invalid opcode"
    end
  end
  data[0]
end

result = execute_program(input_data)

puts result
