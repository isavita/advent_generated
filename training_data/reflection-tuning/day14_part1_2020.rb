def solve_docking_data(filename)
  memory = {}
  mask_set = 0
  mask_clear = 0

  File.foreach(filename) do |line|
    if line.start_with?('mask')
      mask = line.split(' = ').last.strip
      mask_set = mask.tr('X', '0').to_i(2)
      mask_clear = mask.tr('X', '1').to_i(2)
    else
      address, value = line.scan(/\d+/).map(&:to_i)
      masked_value = (value | mask_set) & mask_clear
      memory[address] = masked_value
    end
  end

  memory.values.sum
end

puts solve_docking_data('input.txt')
