
file = File.open("input.txt")
total_count = 0
group_answers = Hash(Char, Bool).new

file.each_line do |line|
  if line.strip.empty?
    total_count += group_answers.size
    group_answers = Hash(Char, Bool).new
  else
    line.strip.each_char do |question|
      group_answers[question] = true
    end
  end
end

total_count += group_answers.size
puts total_count
