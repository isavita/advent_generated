File.open("input.txt", "r") do |file|
  mask = ""
  memory = Hash(Int64, Int64).new(0)
  file.each_line do |line|
    if line.starts_with?("mask")
      mask = line.split(" = ")[1].strip
    elsif line.starts_with?("mem")
      address, value = line.split(" = ")
      address = address.split("[")[1].rchop("]").to_i64
      value = value.to_i64
      result = 0_i64
      mask.each_char_with_index do |c, i|
        if c == 'X'
          result |= (value & (1_i64 << (35 - i)))
        elsif c == '1'
          result |= (1_i64 << (35 - i))
        end
      end
      memory[address] = result
    end
  end
  puts memory.values.sum
end