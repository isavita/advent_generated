
def build_regex(rules, rule = '0', depth = 0)
  return '' if depth > 20 # Prevent infinite recursion for rules 8 and 11

  case rules[rule]
  when '"a"', '"b"'
    rules[rule].delete('"')
  else
    rules[rule].split(' | ').map do |seq|
      seq.split(' ').map { |r| build_regex(rules, r, depth + 1) }.join
    end.join('|').tap { |r| r.insert(0, '(').concat(')') unless r.include?('|').! }
  end
end

input = File.read('input.txt').split("\n\n")
rules_data, messages = input[0].split("\n"), input[1].split("\n")

rules = {}
rules_data.each do |rule|
  number, rule_def = rule.split(': ')
  rules[number] = rule_def
end

# Part 2 adjustments
rules['8'] = '42 | 42 8'
rules['11'] = '42 31 | 42 11 31'

regex = Regexp.new('^' + build_regex(rules) + '$')

puts messages.count { |message| regex.match?(message) }
