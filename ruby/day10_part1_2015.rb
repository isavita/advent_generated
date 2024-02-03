
input = File.read('input.txt').chomp

def look_and_say(input)
  input.gsub(/(.)\1*/) { |match| "#{match.length}#{match[0]}" }
end

40.times { input = look_and_say(input) }

puts input.length
