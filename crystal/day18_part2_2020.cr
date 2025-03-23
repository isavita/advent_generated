
def evaluate_part1(expression : String) : Int64
  stack = [] of Int64
  ops = [] of Char

  expression.each_char do |char|
    case char
    when ' '
      next
    when '('
      ops << char
    when ')'
      while ops.last != '('
        op = ops.pop
        val2 = stack.pop
        val1 = stack.pop
        stack << apply(op, val1, val2)
      end
      ops.pop # Remove the '('
    when '+', '*'
      ops << char
    else
      stack << char.to_i64
    end
  end

  while !ops.empty?
    op = ops.pop
    val2 = stack.pop
    val1 = stack.pop
    stack << apply(op, val1, val2)
  end

  stack[0]
end

def apply(op : Char, val1 : Int64, val2 : Int64) : Int64
  case op
  when '+'
    val1 + val2
  when '*'
    val1 * val2
  else
    raise "Unknown operator: #{op}"
  end
end


def evaluate_part2(expression : String) : Int64
    # First, handle parentheses
    while expression.includes?("(")
        match = expression.match /\(([^()]+)\)/
        if match
            inner_expression = match[1].to_s
            result = evaluate_part2_no_parens(inner_expression)
            expression = expression.sub(/\(([^()]+)\)/, result.to_s)
        else
            raise "Unbalanced parentheses"
        end
    end
    evaluate_part2_no_parens(expression)
end

def evaluate_part2_no_parens(expression : String) : Int64
    # Addition first
    while expression.includes?("+")
        match = expression.match /(\d+) \+ (\d+)/
        if match
            val1 = match[1].to_i64
            val2 = match[2].to_i64
            result = val1 + val2
            expression = expression.sub(/(\d+) \+ (\d+)/, result.to_s)
        else
            raise "Invalid addition"
        end
    end

    # Multiplication
    result = 1_i64
    expression.split(" * ").each do |num_str|
        result *= num_str.to_i64
    end
    result
end



def main
  total_part1 = 0_i64
  total_part2 = 0_i64
  File.each_line("input.txt") do |line|
    line = line.strip
    total_part1 += evaluate_part1(line)
    total_part2 += evaluate_part2(line)
  end

  puts "Part 1: #{total_part1}"
  puts "Part 2: #{total_part2}"
end

main
