
def is_number?(str)
  true if Float(str) rescue false
end

def get_value(registers, val)
  is_number?(val) ? val.to_i : registers[val]
end

def run_program(instructions, registers)
  pc = 0

  while pc < instructions.length
    instr, x, y = instructions[pc].split

    case instr
    when 'cpy'
      registers[y] = get_value(registers, x)
    when 'inc'
      registers[x] += 1
    when 'dec'
      registers[x] -= 1
    when 'jnz'
      if get_value(registers, x) != 0
        pc += get_value(registers, y)
        next
      end
    when 'tgl'
      toggle_index = pc + get_value(registers, x)
      if toggle_index >= 0 && toggle_index < instructions.length
        toggle_instr = instructions[toggle_index].split
        if toggle_instr.length == 2
          toggle_instr[0] = toggle_instr[0] == 'inc' ? 'dec' : 'inc'
        elsif toggle_instr.length == 3
          toggle_instr[0] = toggle_instr[0] == 'jnz' ? 'cpy' : 'jnz'
        end
        instructions[toggle_index] = toggle_instr.join(' ')
      end
    end

    pc += 1
  end
end

input = File.readlines('input.txt').map(&:strip)
registers = { 'a' => 7, 'b' => 0, 'c' => 0, 'd' => 0 }

run_program(input, registers)

puts registers['a']
