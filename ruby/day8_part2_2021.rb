
def decode_signals(input)
  input.map do |line|
    patterns, output = line.split(' | ').map(&:split)
    digits = {}
    patterns.each do |pattern|
      case pattern.length
      when 2 then digits[1] = pattern.chars.sort.join
      when 3 then digits[7] = pattern.chars.sort.join
      when 4 then digits[4] = pattern.chars.sort.join
      when 7 then digits[8] = pattern.chars.sort.join
      end
    end
    patterns = patterns.map { |pattern| pattern.chars.sort.join }
    digits[3] = patterns.find { |pattern| pattern.length == 5 && (digits[1].chars - pattern.chars).empty? }
    digits[9] = patterns.find { |pattern| pattern.length == 6 && (digits[4].chars - pattern.chars).empty? }
    digits[0] = patterns.find { |pattern| pattern.length == 6 && pattern != digits[9] && (digits[1].chars - pattern.chars).empty? }
    digits[6] = patterns.find { |pattern| pattern.length == 6 && pattern != digits[9] && pattern != digits[0] }
    digits[5] = patterns.find { |pattern| pattern.length == 5 && (pattern.chars - digits[6].chars).length == 0 }
    digits[2] = patterns.find { |pattern| pattern.length == 5 && pattern != digits[3] && pattern != digits[5] }

    output.map { |o| digits.key(o.chars.sort.join) }.join.to_i
  end.sum
end

input = File.readlines('input.txt').map(&:chomp)
puts decode_signals(input)
