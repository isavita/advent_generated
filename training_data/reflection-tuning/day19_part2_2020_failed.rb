def parse_rules(rules_input)
  rules = {}
  rules_input.each do |line|
    num, rule = line.split(': ')
    rules[num.to_i] = rule.gsub('"', '')
  end
  rules
end

def build_regex(rules, rule_num, part2 = false)
  rule = rules[rule_num]
  return rule if rule =~ /^[ab]$/

  if part2
    return "(?:#{build_regex(rules, 42)})+" if rule_num == 8
    if rule_num == 11
      # This is a simplification and might not work for all inputs
      return "(?:#{build_regex(rules, 42)}{1}#{build_regex(rules, 31)}{1}|" +
             "#{build_regex(rules, 42)}{2}#{build_regex(rules, 31)}{2}|" +
             "#{build_regex(rules, 42)}{3}#{build_regex(rules, 31)}{3}|" +
             "#{build_regex(rules, 42)}{4}#{build_regex(rules, 31)}{4})"
    end
  end

  options = rule.split(' | ')
  "(#{options.map { |option| option.split.map { |r| build_regex(rules, r.to_i, part2) }.join }.join('|')})"
end

def count_matches(messages, regex)
  messages.count { |msg| msg.match?(/^#{regex}$/) }
end

rules_input, messages = File.read('input.txt').split("\n\n").map(&:lines)
rules = parse_rules(rules_input)

# Part 1
regex_part1 = build_regex(rules, 0)
part1_result = count_matches(messages, regex_part1)
puts "Part 1: #{part1_result}"

# Part 2
regex_part2 = build_regex(rules, 0, true)
part2_result = count_matches(messages, regex_part2)
puts "Part 2: #{part2_result}"
