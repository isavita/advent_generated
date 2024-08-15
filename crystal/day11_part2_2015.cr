require "file_utils"

def read_input(filename)
  File.read(filename)
end

def increment_password(password)
  password = password.chars
  (password.size - 1).downto(0) do |i|
    if password[i] == 'z'
      password[i] = 'a'
    else
      password[i] = (password[i].ord + 1).chr
      break
    end
  end
  password.join
end

def is_valid_password(password)
  has_straight(password) && !contains_invalid_letters(password) && has_two_pairs(password)
end

def has_straight(password)
  0.upto(password.size - 3) do |i|
    return true if password[i].ord.succ == password[i.succ].ord && password[i].ord.succ.succ == password[i.succ.succ].ord
  end
  false
end

def contains_invalid_letters(password)
  invalid_letters = ["i", "o", "l"]
  invalid_letters.any? { |letter| password.includes? letter }
end

def has_two_pairs(password)
  count = 0
  prev_char = nil
  password.each_char do |char|
    if char == prev_char
      count += 1
      prev_char = nil
    else
      prev_char = char
    end
    break if count == 2
  end
  count == 2
end

def find_next_password(password)
  loop do
    password = increment_password(password)
    break password if is_valid_password(password)
  end
end

current_password = read_input("input.txt").strip
first_new_password = find_next_password(current_password)
second_new_password = find_next_password(first_new_password)
puts second_new_password