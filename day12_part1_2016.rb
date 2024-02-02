
def run_assembunny(instructions)
  registers = { 'a' => 0, 'b' => 0, 'c' => 0, 'd' => 0 }
  pc = 0

  while pc < instructions.length
    instr, x, y = instructions[pc]

    case instr
    when 'cpy'
      registers[y] = registers.key?(x) ? registers[x] : x.to_i
    when 'inc'
      registers[x] += 1
    when 'dec'
      registers[x] -= 1
    when 'jnz'
      if registers.key?(x) ? registers[x] != 0 : x.to_i != 0
        pc += y.to_i - 1
      end
    end

    pc += 1
  end

  registers['a']
end

instructions = File.readlines('input.txt').map { |line| line.split }

puts run_assembunny(instructions)
