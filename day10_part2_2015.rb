
def look_and_say(input)
  input.gsub(/(.)\1*/) { |match| "#{match.length}#{match[0]}" }
end

input = File.read('input.txt').chomp
40.times { input = look_and_say(input) }
puts input.length

input = File.read('input.txt').chomp
50.times { input = look_and_say(input) }
puts input.length
