def execute_program(instructions, registers)
  ip = 0
  while ip < instructions.length
    inst, *args = instructions[ip].split
    case inst
    when 'cpy'
      registers[args[1]] = args[0].to_i.to_s == args[0] ? args[0].to_i : registers[args[0]]
    when 'inc'
      registers[args[0]] += 1
    when 'dec'
      registers[args[0]] -= 1
    when 'jnz'
      value = args[0].to_i.to_s == args[0] ? args[0].to_i : registers[args[0]]
      ip += args[1].to_i - 1 if value != 0
    when 'tgl'
      target = ip + (args[0].to_i.to_s == args[0] ? args[0].to_i : registers[args[0]])
      if target >= 0 && target < instructions.length
        toggled_inst = instructions[target].split
        case toggled_inst.length
        when 2
          toggled_inst[0] = toggled_inst[0] == 'inc' ? 'dec' : 'inc'
        when 3
          toggled_inst[0] = toggled_inst[0] == 'jnz' ? 'cpy' : 'jnz'
        end
        instructions[target] = toggled_inst.join(' ')
      end
    end
    ip += 1
  end
end

instructions = File.readlines('input.txt').map(&:chomp)
registers = Hash.new(0)
registers['a'] = 7  # Set initial value of register 'a' to 7 (number of eggs)

execute_program(instructions, registers)
puts registers['a']
