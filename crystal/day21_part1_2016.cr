
def swap_position(password : String, x : Int32, y : Int32) : String
  chars = password.chars
  chars[x], chars[y] = chars[y], chars[x]
  chars.join
end

def swap_letter(password : String, x : Char, y : Char) : String
  password.tr(x.to_s + y.to_s, y.to_s + x.to_s)
end

def rotate_left(password : String, steps : Int32) : String
  steps = steps % password.size
  password[steps..-1] + password[0...steps]
end

def rotate_right(password : String, steps : Int32) : String
  steps = steps % password.size
  password[-steps..-1] + password[0...-steps]
end

def rotate_based_on_letter(password : String, letter : Char) : String
  index = password.index(letter).not_nil!
  rotate_steps = 1 + index + (index >= 4 ? 1 : 0)
  rotate_right(password, rotate_steps)
end

def reverse_positions(password : String, x : Int32, y : Int32) : String
  chars = password.chars
  chars[x..y] = chars[x..y].reverse
  chars.join
end

def move_position(password : String, x : Int32, y : Int32) : String
  chars = password.chars
  char = chars.delete_at(x)
  chars.insert(y, char)
  chars.join
end

def scramble_password(password : String, instructions : Array(String)) : String
  instructions.each do |instruction|
    parts = instruction.split
    case parts[0]
    when "swap"
      if parts[1] == "position"
        password = swap_position(password, parts[2].to_i, parts[5].to_i)
      else
        password = swap_letter(password, parts[2][0], parts[5][0])
      end
    when "rotate"
      if parts[1] == "left"
        password = rotate_left(password, parts[2].to_i)
      elsif parts[1] == "right"
        password = rotate_right(password, parts[2].to_i)
      else
        password = rotate_based_on_letter(password, parts[6][0])
      end
    when "reverse"
      password = reverse_positions(password, parts[2].to_i, parts[4].to_i)
    when "move"
      password = move_position(password, parts[2].to_i, parts[5].to_i)
    end
  end
  password
end

# Read input from file
instructions = File.read_lines("input.txt")

# Scramble the password
initial_password = "abcdefgh"
scrambled_password = scramble_password(initial_password, instructions)

# Print the result
puts scrambled_password
