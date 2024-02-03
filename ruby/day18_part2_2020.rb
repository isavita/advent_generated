
def evaluate_expression(expression)
  while expression.include?('(')
    expression.gsub!(/\([^()]*\)/) { |match| evaluate_expression(match[1..-2]) }
  end
  evaluate_simple_expression(expression)
end

def evaluate_simple_expression(expression)
  numbers = expression.scan(/\d+/).map(&:to_i)
  operators = expression.scan(/[+*]/)
  result = numbers.shift
  operators.each_with_index do |op, i|
    result = op == '+' ? result + numbers[i] : result * numbers[i]
  end
  result
end

def part_one
  File.readlines('input.txt').sum { |line| evaluate_expression(line.chomp) }
end

def evaluate_advanced_expression(expression)
  while expression.include?('(')
    expression.gsub!(/\([^()]*\)/) { |match| evaluate_advanced_expression(match[1..-2]) }
  end
  evaluate_advanced_simple_expression(expression)
end

def evaluate_advanced_simple_expression(expression)
  while expression.include?('+')
    expression.gsub!(/\d+ \+ \d+/) { |match| match.split(' + ').map(&:to_i).sum }
  end
  expression.split(' * ').map(&:to_i).reduce(:*)
end

def part_two
  File.readlines('input.txt').sum { |line| evaluate_advanced_expression(line.chomp) }
end

puts part_one
puts part_two
