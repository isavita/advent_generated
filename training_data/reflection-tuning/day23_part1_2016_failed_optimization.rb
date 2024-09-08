def solve(input)
  instructions = input.split("\n").map(&:split)
  registers = Hash.new(0)
  registers['a'] = 7  # Set initial value of register 'a' to 7 (number of eggs)
  pc = 0  # Program counter

  while pc < instructions.length
    op, *args = instructions[pc]

    case op
    when 'cpy'
      value = args[0].to_i.to_s == args[0] ? args[0].to_i : registers[args[0]]
      registers[args[1]] = value
    when 'inc'
      registers[args[0]] += 1
    when 'dec'
      registers[args[0]] -= 1
    when 'jnz'
      test = args[0].to_i.to_s == args[0] ? args[0].to_i : registers[args[0]]
      pc += args[1].to_i - 1 if test != 0
    when 'tgl'
      target = pc + (args[0].to_i.to_s == args[0] ? args[0].to_i : registers[args[0]])
      if target >= 0 && target < instructions.length
        instr = instructions[target]
        case instr.length
        when 2
          instr[0] = instr[0] == 'inc' ? 'dec' : 'inc'
        when 3
          instr[0] = instr[0] == 'jnz' ? 'cpy' : 'jnz'
        end
      end
    end

    pc += 1
  end

  registers['a']
end

# Example usage:
# input = File.read('input.txt')
# result = solve(input)
# puts result
