
keypad = {
  '1' => {'D' => '3'},
  '2' => {'R' => '3', 'D' => '6'},
  '3' => {'L' => '2', 'D' => '7', 'R' => '4', 'U' => '1'},
  '4' => {'L' => '3', 'D' => '8'},
  '5' => {'R' => '6'},
  '6' => {'L' => '5', 'D' => 'A', 'R' => '7', 'U' => '2'},
  '7' => {'L' => '6', 'D' => 'B', 'R' => '8', 'U' => '3'},
  '8' => {'L' => '7', 'D' => 'C', 'R' => '9', 'U' => '4'},
  '9' => {'L' => '8'},
  'A' => {'U' => '6', 'R' => 'B'},
  'B' => {'L' => 'A', 'D' => 'D', 'R' => 'C', 'U' => '7'},
  'C' => {'L' => 'B'},
  'D' => {'U' => 'B'}
}

code = ''
current_key = '5'

File.open('input.txt').each do |line|
  line.chomp.each_char do |char|
    current_key = keypad[current_key][char] if keypad[current_key].key?(char)
  end
  code += current_key
end

puts code
