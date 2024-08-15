require "file"

def read_rules(file)
  rules = {} of Int32 => String
  file.each_line do |line|
    if line.empty?
      break
    end
    parts = line.split(": ")
    rules[parts[0].to_i] = parts[1].gsub("\"", "")
  end
  rules
end

def construct_pattern(rules, index)
  if rules[index].includes? "|"
    subrules = rules[index].split(" | ")
    parts = [] of String
    subrules.each do |subrule|
      parts << construct_sub_pattern(rules, subrule)
    end
    "(" + parts.join("|") + ")"
  else
    construct_sub_pattern(rules, rules[index])
  end
end

def construct_sub_pattern(rules, subrule)
  if subrule == "a" || subrule == "b"
    subrule
  else
    sub_idxs = subrule.split(" ")
    pattern = ""
    sub_idxs.each do |idx|
      pattern += construct_pattern(rules, idx.to_i)
    end
    pattern
  end
end

def count_matches(file, pattern)
  count = 0
  regex = Regex.new("^#{pattern}$")
  file.rewind
  file.each_line do |message|
    count += 1 if regex.match(message.chomp)
  end
  count
end

file = File.open("input.txt")
rules = read_rules(file)
pattern = construct_pattern(rules, 0)
count = count_matches(file, pattern)

puts "The number of messages that completely match rule 0 is: #{count}"