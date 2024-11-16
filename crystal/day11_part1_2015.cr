
def read_input(filename : String) : String?
  File.read(filename)
rescue
  nil
end

def increment_password(password : String) : String
  chars = password.chars.map { |c| c }
  i = chars.size - 1
  loop do
    chars[i] = (chars[i].ord + 1).chr
    if chars[i] > 'z'
      chars[i] = 'a'
      i -= 1
    else
      break
    end
  end
  chars.join
end

def valid_password?(password : String) : Bool
  has_straight?(password) && !contains_invalid_letters?(password) && has_two_pairs?(password)
end

def has_straight?(password : String) : Bool
  password.each_char.with_index do |c, i|
    return true if i + 2 < password.size && c.succ == password[i + 1] && password[i + 1].succ == password[i + 2]
  end
  false
end

def contains_invalid_letters?(password : String) : Bool
  password.each_char.any? { |c| ['i', 'o', 'l'].includes?(c) }
end

def has_two_pairs?(password : String) : Bool
  count = 0
  i = 0
  while i < password.size - 1
    if password[i] == password[i + 1]
      count += 1
      i += 2
    else
      i += 1
    end
  end
  count >= 2
end

current_password = read_input("input.txt") || abort("Error reading input")
new_password = loop do
  current_password = increment_password(current_password)
  break current_password if valid_password?(current_password)
end

puts new_password
