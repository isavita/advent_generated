
def increment_password(password)
  password.next
end

def valid_password?(password)
  password =~ /(?=.*([a-z])\1.*([a-z])\2)(?!.*[iol])(?=.*abc|.*bcd|.*cde|.*def|.*efg|.*fgh|.*pqr|.*qrs|.*rst|.*stu|.*tuv|.*uvw|.*vwx|.*wxy|.*xyz)/
end

def find_next_password(password)
  loop do
    password = increment_password(password)
    return password if valid_password?(password)
  end
end

current_password = File.read('input.txt').strip
new_password = find_next_password(current_password)
puts new_password
