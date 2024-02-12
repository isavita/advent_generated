
struct Rule
  property name : String
  property ranges : Array(Tuple(Int32, Int32))

  def initialize(@name : String, @ranges : Array(Tuple(Int32, Int32)))
  end

  def is_valid(value : Int32) : Bool
    @ranges.any? { |range| value >= range[0] && value <= range[1] }
  end
end

def to_int(s : String) : Int32
  s.to_i
end

def is_valid_for_any_rule(value : Int32, rules : Array(Rule)) : Bool
  rules.any? { |rule| rule.is_valid(value) }
end

rules = [] of Rule
scanning_rules = true
error_rate = 0

File.open("input.txt") do |file|
  file.each_line do |line|
    next if line.strip.empty?

    if line.starts_with?("your ticket:") || line.starts_with?("nearby tickets:")
      scanning_rules = false
      next
    end

    if scanning_rules
      matches = line.match(/^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$/)
      next unless matches

      name = matches[1]
      range1 = { matches[2].to_i, matches[3].to_i }
      range2 = { matches[4].to_i, matches[5].to_i }
      rules << Rule.new(name, [range1, range2])
    else
      line.split(",").each do |value|
        val = to_int(value)
        error_rate += val unless is_valid_for_any_rule(val, rules)
      end
    end
  end
end

puts error_rate
