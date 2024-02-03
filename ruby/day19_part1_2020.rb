
rules, messages = File.read("input.txt").split("\n\n")

rules = rules.split("\n").map { |rule| rule.split(": ") }
messages = messages.split("\n")

def build_regex(rules, rule_id, rules_hash)
  return rules_hash[rule_id] if rules_hash[rule_id]

  rule = rules.find { |r| r[0] == rule_id }[1]

  if rule.include?("\"")
    rules_hash[rule_id] = rule.delete("\"")
  else
    sub_rules = rule.split(" | ").map do |sub_rule|
      sub_rule.split(" ").map do |id|
        build_regex(rules, id, rules_hash)
      end.join
    end

    rules_hash[rule_id] = "(#{sub_rules.join("|")})"
  end
end

rules_hash = {}
build_regex(rules, "0", rules_hash)

regex = Regexp.new("^#{rules_hash["0"]}$")

puts messages.count { |message| regex.match?(message) }
