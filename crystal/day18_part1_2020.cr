
def evaluate(expression : String) : Int64
  tokens = expression.gsub("(", "( ").gsub(")", " )").split
  evaluate_tokens(tokens)
end

def evaluate_tokens(tokens : Array(String)) : Int64
  ops = [] of String
  vals = [] of Int64

  tokens.each do |token|
    case token
    when "("
      ops << token
    when "+", "*"
      while ops.size > 0 && ops.last != "("
        vals = vals[0, vals.size - 2] << apply_op(ops.pop, vals[-2], vals[-1])
      end
      ops << token
    when ")"
      while ops.last != "("
        vals = vals[0, vals.size - 2] << apply_op(ops.pop, vals[-2], vals[-1])
      end
      ops.pop
    else
      vals << token.to_i64
    end
  end

  while ops.size > 0
    vals = vals[0, vals.size - 2] << apply_op(ops.pop, vals[-2], vals[-1])
  end
  vals[0]
end

def apply_op(op : String, a : Int64, b : Int64) : Int64
  case op
  when "+"
    a + b
  when "*"
    a * b
  else
    raise "Unknown operator: #{op}"
  end
end

sum = 0_i64
File.open("input.txt") do |file|
  file.each_line do |line|
    sum += evaluate(line.strip)
  end
end

puts sum
