
def evaluate_expression(expression)
  while expression.include?('(')
    expression.gsub!(/\([^()]*\)/) { |match| evaluate_expression(match[1..-2]) }
  end
  evaluate_simple_expression(expression)
end

def evaluate_simple_expression(expression)
  tokens = expression.split(' ')
  result = tokens.shift.to_i
  until tokens.empty?
    operator = tokens.shift
    next_number = tokens.shift.to_i
    result = operator == '+' ? result + next_number : result * next_number
  end
  result
end

def solve_homework(file_path)
  File.readlines(file_path).sum { |line| evaluate_expression(line.chomp) }
end

puts solve_homework('input.txt')
