def parse_input(filename)
  rules, messages = File.read(filename).split("\n\n")
  rules = rules.split("\n").map { |r| r.split(": ") }.to_h
  messages = messages.split("\n")
  [rules, messages]
end

def match(rules, rule_num, message, pos, memo)
  key = [rule_num, pos]
  return memo[key] if memo.key?(key)

  rule = rules[rule_num]
  if rule.start_with?('"')
    result = message[pos] == rule[1] ? [pos + 1] : []
  else
    result = rule.split(" | ").flat_map do |option|
      new_pos = [pos]
      option.split.each do |sub_rule|
        new_pos = new_pos.flat_map { |p| match(rules, sub_rule, message, p, memo) }
        break [] if new_pos.empty?
      end
      new_pos
    end
  end

  memo[key] = result
  result
end

rules, messages = parse_input('input.txt')
count = messages.count do |message|
  memo = {}
  match(rules, '0', message, 0, memo).any? { |pos| pos == message.length }
end

puts count
