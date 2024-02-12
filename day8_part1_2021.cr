
File.open("input.txt") do |file|
  count = 0
  file.each_line do |line|
    parts = line.strip.split(" | ")
    _, output = parts[0], parts[1]
    output.split(" ").each do |digit|
      case digit.size
      when 2, 4, 3, 7
        count += 1
      end
    end
  end

  puts count
end
