def evaluate_simple(tokens, part2 = false)
  if part2
    products = tokens.join(' ').split('*').map { |expr| expr.split('+').map(&:to_i).sum }
    products.inject(:*)
  else
    result = tokens[0].to_i
    tokens[1..-1].each_slice(2) do |op, num|
      result = op == '+' ? result + num.to_i : result * num.to_i
    end
    result
  end
end

def evaluate(expression, part2 = false)
  tokens = expression.gsub('(', '( ').gsub(')', ' )').split
  stack = []
  current = []

  tokens.each do |token|
    if token == '('
      stack.push(current)
      current = []
    elsif token == ')'
      result = evaluate_simple(current, part2)
      current = stack.pop
      current.push(result.to_s)
    else
      current.push(token)
    end
  end

  evaluate_simple(current, part2)
end

input = File.read('input.txt').split("\n")
part1_sum = input.sum { |expr| evaluate(expr) }
part2_sum = input.sum { |expr| evaluate(expr, true) }

puts "Part 1: #{part1_sum}"
puts "Part 2: #{part2_sum}"
