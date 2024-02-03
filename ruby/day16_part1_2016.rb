
data = File.read('input.txt').chomp

def dragon_curve(data)
  a = data
  b = a.reverse.chars.map { |c| c == '0' ? '1' : '0' }.join
  "#{a}0#{b}"
end

def checksum(data)
  checksum = data.chars.each_slice(2).map { |a, b| a == b ? '1' : '0' }.join
  checksum.length.even? ? checksum(checksum) : checksum
end

generated_data = data
while generated_data.length < 272
  generated_data = dragon_curve(generated_data)
end

generated_data = generated_data[0..271]

puts checksum(generated_data)
