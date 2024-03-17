file = File.open("input.txt")
total_count = 0
group_answers = Hash(Char, Int32).new
group_size = 0

file.each_line do |line|
  line = line.chomp
  if line.empty?
    group_answers.each do |_, count|
      total_count += 1 if count == group_size
    end
    group_answers.clear
    group_size = 0
  else
    group_size += 1
    line.each_char do |question|
      group_answers[question] ||= 0
      group_answers[question] += 1
    end
  end
end

group_answers.each do |_, count|
  total_count += 1 if count == group_size
end

puts total_count