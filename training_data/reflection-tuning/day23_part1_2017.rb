def execute_program(instructions)
  registers = Hash.new(0)
  pc = 0
  mul_count = 0

  while pc < instructions.length
    op, x, y = instructions[pc].split
    y = y =~ /[a-h]/ ? registers[y] : y.to_i

    case op
    when 'set'
      registers[x] = y
    when 'sub'
      registers[x] -= y
    when 'mul'
      registers[x] *= y
      mul_count += 1
    when 'jnz'
      x_val = x =~ /[a-h]/ ? registers[x] : x.to_i
      pc += y - 1 if x_val != 0
    end

    pc += 1
  end

  mul_count
end

instructions = File.readlines('input.txt', chomp: true)
puts execute_program(instructions)
