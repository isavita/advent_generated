
class Rule
  property workflow_name : String, category : String, operator : String, num : Int32

  def initialize(@workflow_name : String, @category : String, @operator : String, @num : Int32)
  end
end

class Workflow
  property name : String, rules : Array(Rule)

  def initialize(@name : String, @rules : Array(Rule))
  end
end

class PartInterval
  property x : Tuple(Int32, Int32), m : Tuple(Int32, Int32), a : Tuple(Int32, Int32), s : Tuple(Int32, Int32)

  def initialize(@x : Tuple(Int32, Int32), @m : Tuple(Int32, Int32), @a : Tuple(Int32, Int32), @s : Tuple(Int32, Int32))
  end
end

def parse_workflows(input : Array(String)) : Hash(String, Array(Rule))
  workflows = Hash(String, Array(Rule)).new
  input.each do |line|
    break if line.empty?
    idx = line.index('{').not_nil!
    name = line[0, idx]
    rules = [] of Rule
    rules_str = line[idx + 1, line.size - idx - 2].split(',')
    rules_str.each do |rule_str|
      colon = rule_str.index(':')
      if colon.nil?
        rules << Rule.new(rule_str, "", "", 0)
      else
        category = rule_str[0].to_s
        operator = rule_str[1].to_s
        num = rule_str[2, colon - 2].to_i
        target = rule_str[colon + 1, rule_str.size - colon - 1]
        rules << Rule.new(target, category, operator, num)
      end
    end
    workflows[name] = rules
  end
  workflows
end

def apply_interval(pi : PartInterval, workflows : Hash(String, Array(Rule)), name : String) : Int64
  return 0_i64 if name == "R"
  if name == "A"
    return (pi.x[1] - pi.x[0] + 1).to_i64 *
           (pi.m[1] - pi.m[0] + 1).to_i64 *
           (pi.a[1] - pi.a[0] + 1).to_i64 *
           (pi.s[1] - pi.s[0] + 1).to_i64
  end
  rules = workflows[name]
  x, m, a, s = pi.x, pi.m, pi.a, pi.s
  total = 0_i64
  rules.each do |rule|
    if rule.category.empty?
      total += apply_interval(PartInterval.new(x, m, a, s), workflows, rule.workflow_name)
      break
    end
    lo, hi = 0, 0
    case rule.category
    when "x" then lo, hi = x
    when "m" then lo, hi = m
    when "a" then lo, hi = a
    when "s" then lo, hi = s
    end
    valid_lo, valid_hi, invalid_lo, invalid_hi = lo, hi, lo, hi
    case rule.operator
    when ">"
      valid_lo = rule.num + 1
      invalid_hi = rule.num
    when "<"
      valid_hi = rule.num - 1
      invalid_lo = rule.num
    end
    next if valid_lo > valid_hi
    vx, vm, va, vs = x, m, a, s
    case rule.category
    when "x" then vx = {valid_lo, valid_hi}
    when "m" then vm = {valid_lo, valid_hi}
    when "a" then va = {valid_lo, valid_hi}
    when "s" then vs = {valid_lo, valid_hi}
    end
    total += apply_interval(PartInterval.new(vx, vm, va, vs), workflows, rule.workflow_name)
    case rule.category
    when "x" then x = {invalid_lo, invalid_hi}
    when "m" then m = {invalid_lo, invalid_hi}
    when "a" then a = {invalid_lo, invalid_hi}
    when "s" then s = {invalid_lo, invalid_hi}
    end
  end
  total
end

input = File.read("input.txt").lines.map(&.strip).reject(&.empty?)
workflows = parse_workflows(input)
pi = PartInterval.new({1, 4000}, {1, 4000}, {1, 4000}, {1, 4000})
puts apply_interval(pi, workflows, "in")
