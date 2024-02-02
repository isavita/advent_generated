
data = File.readlines('input.txt').map(&:chomp)

def calculate_number(monkey, monkeys)
  return monkey.to_i if monkey.match?(/\A\d+\z/)

  operation = monkey.split(' ')
  case operation[1]
  when '+'
    return calculate_number(monkeys[operation[0]], monkeys) + calculate_number(monkeys[operation[2]], monkeys)
  when '-'
    return calculate_number(monkeys[operation[0]], monkeys) - calculate_number(monkeys[operation[2]], monkeys)
  when '*'
    return calculate_number(monkeys[operation[0]], monkeys) * calculate_number(monkeys[operation[2]], monkeys)
  when '/'
    return calculate_number(monkeys[operation[0]], monkeys) / calculate_number(monkeys[operation[2]], monkeys)
  end
end

monkeys = {}
data.each do |line|
  monkey, job = line.split(': ')
  monkeys[monkey] = job
end

root_number = calculate_number(monkeys['root'], monkeys)
puts root_number
