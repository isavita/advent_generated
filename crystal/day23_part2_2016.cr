
def is_register(x : String)
  'a' <= x[0] <= 'd'
end

def get_value(x : String, registers : Hash(Char, Int32))
  if is_register(x)
    registers[x[0]]
  else
    x.to_i
  end
end

def execute_program(instructions : Array(String), registers : Hash(Char, Int32))
  i = 0
  while i < instructions.size
    if i + 5 < instructions.size
      pattern = instructions[i, 6]
      if pattern[0].starts_with?("cpy") &&
         pattern[1].starts_with?("inc") &&
         pattern[2].starts_with?("dec") &&
         pattern[3].starts_with?("jnz") &&
         pattern[4].starts_with?("dec") &&
         pattern[5].starts_with?("jnz")
        cpy_x, cpy_y = pattern[0].split[1], pattern[0].split[2]
        inc_a = pattern[1].split[1]
        dec_c = pattern[2].split[1]
        jnz_c, jnz_c_offset = pattern[3].split[1], pattern[3].split[2]
        dec_d = pattern[4].split[1]
        jnz_d, jnz_d_offset = pattern[5].split[1], pattern[5].split[2]

        if inc_a == "a" && dec_c == cpy_y && jnz_c == cpy_y && jnz_c_offset.to_i == -2 &&
           dec_d == "d" && jnz_d == "d" && jnz_d_offset.to_i == -5
          registers['a'] += registers[cpy_x[0]] * registers['d']
          registers[cpy_y[0]] = 0
          registers['d'] = 0
          i += 6
          next
        end
      end
    end

    parts = instructions[i].split
    cmd = parts[0]

    if cmd == "tgl"
      x = get_value(parts[1], registers)
      target_idx = i + x
      if 0 <= target_idx < instructions.size
        target_parts = instructions[target_idx].split
        if target_parts.size == 2
          if target_parts[0] == "inc"
            target_parts[0] = "dec"
          else
            target_parts[0] = "inc"
          end
        elsif target_parts.size == 3
          if target_parts[0] == "jnz"
            target_parts[0] = "cpy"
          else
            target_parts[0] = "jnz"
          end
        end
        instructions[target_idx] = target_parts.join(" ")
      end
      i += 1
      next
    end

    if cmd == "cpy"
      x, y = parts[1], parts[2]
      if is_register(y)
        registers[y[0]] = get_value(x, registers)
      end
      i += 1
    elsif cmd == "inc"
      x = parts[1]
      if is_register(x)
        registers[x[0]] += 1
      end
      i += 1
    elsif cmd == "dec"
      x = parts[1]
      if is_register(x)
        registers[x[0]] -= 1
      end
      i += 1
    elsif cmd == "jnz"
      x, y = parts[1], parts[2]
      if get_value(x, registers) != 0
        i += get_value(y, registers)
      else
        i += 1
      end
    else
      i += 1
    end
  end
end

def main
  instructions = [] of String
  File.open("./input.txt") do |file|
    file.each_line do |line|
      instructions << line.strip
    end
  end
  registers = {'a' => 12, 'b' => 0, 'c' => 0, 'd' => 0}.to_h
  execute_program(instructions, registers)
  puts registers['a']
end

main
