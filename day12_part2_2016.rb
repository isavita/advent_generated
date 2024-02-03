
def run_assembunny(input, c_value = 0)
  registers = {'a' => 0, 'b' => 0, 'c' => c_value, 'd' => 0}
  instructions = input.split("\n")
  i = 0
  while i < instructions.length
    parts = instructions[i].split
    case parts[0]
    when 'cpy'
      value = parts[1].match?(/\d+/) ? parts[1].to_i : registers[parts[1]]
      registers[parts[2]] = value
    when 'inc'
      registers[parts[1]] += 1
    when 'dec'
      registers[parts[1]] -= 1
    when 'jnz'
      value = parts[1].match?(/\d+/) ? parts[1].to_i : registers[parts[1]]
      i += parts[2].to_i - 1 if value != 0
    end
    i += 1
  end
  registers['a']
end

input = File.read('input.txt')
puts run_assembunny(input) # Part 1
puts run_assembunny(input, 1) # Part 2
