def tokenize(expression)
  expression.gsub('(', ' ( ').gsub(')', ' ) ').split
end

def evaluate(tokens)
  stack = []
  result = 0
  operator = '+'

  tokens.each do |token|
    case token
    when '('
      sub_expr = []
      paren_count = 1
      while paren_count > 0
        t = tokens.shift
        if t == '('
          paren_count += 1
        elsif t == ')'
          paren_count -= 1
        end
        sub_expr << t unless paren_count == 0
      end
      stack << evaluate(sub_expr)
    when '+'
      operator = '+'
    when '*'
      operator = '*'
    else
      num = token.to_i
      case operator
      when '+'
        result += num
      when '*'
        result *= num
      end
    end
  end

  result
end

total = 0
File.readlines('input.txt').each do |line|
  tokens = tokenize(line.strip)
  total += evaluate(tokens)
end

puts total
