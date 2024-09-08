def get_value(registers, x)
  x.match?(/[a-z]/) ? registers[x] : x.to_i
end

registers = Hash.new(0)
instructions = File.readlines('input.txt').map(&:split)
last_sound = nil
i = 0

while i >= 0 && i < instructions.length
  cmd, x, y = instructions[i]
  y_val = get_value(registers, y) if y

  case cmd
  when 'snd'
    last_sound = get_value(registers, x)
  when 'set'
    registers[x] = y_val
  when 'add'
    registers[x] += y_val
  when 'mul'
    registers[x] *= y_val
  when 'mod'
    registers[x] %= y_val
  when 'rcv'
    if get_value(registers, x) != 0
      puts last_sound
      break
    end
  when 'jgz'
    i += y_val - 1 if get_value(registers, x) > 0
  end

  i += 1
end
