def apply_operation(op, password)
  fields = op.split
  case fields[0]
  when "swap"
    case fields[1]
    when "position"
      x, y = fields[2].to_i, fields[5].to_i
      password = swap_position(password, x, y)
    when "letter"
      x, y = fields[2], fields[5]
      password = swap_letter(password, x, y)
    end
  when "rotate"
    case fields[1]
    when "left"
      steps = fields[2].to_i
      password = rotate_left(password, steps)
    when "right"
      steps = fields[2].to_i
      password = rotate_right(password, steps)
    when "based"
      x = fields[6]
      password = rotate_based_on_position(password, x)
    end
  when "reverse"
    x, y = fields[2].to_i, fields[4].to_i
    password = reverse_positions(password, x, y)
  when "move"
    x, y = fields[2].to_i, fields[5].to_i
    password = move_position(password, x, y)
  end
  password
end

def swap_position(password, x, y)
  password[x], password[y] = password[y], password[x]
  password
end

def swap_letter(password, x, y)
  password.tr!(x+y, y+x)
  password
end

def rotate_left(password, steps)
  steps %= password.length
  password[steps..-1] + password[0...steps]
end

def rotate_right(password, steps)
  steps %= password.length
  password[-steps..-1] + password[0...-steps]
end

def rotate_based_on_position(password, x)
  index = password.index(x)
  steps = 1 + index
  steps += 1 if index >= 4
  rotate_right(password, steps)
end

def reverse_positions(password, x, y)
  password[x..y] = password[x..y].reverse
  password
end

def move_position(password, x, y)
  char = password.slice!(x)
  password.insert(y, char)
end

password = "abcdefgh"
operations = File.readlines('input.txt').map(&:chomp)

operations.each do |op|
  password = apply_operation(op, password)
end

puts password