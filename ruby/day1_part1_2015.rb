
input = File.read('input.txt').strip
floor = input.count('(') - input.count(')')
puts floor
