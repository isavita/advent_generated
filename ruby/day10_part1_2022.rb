File.open("input.txt", "r") do |file|
  x = [1]
  file.each_line do |line|
    case line.strip
    when "noop"
      x << x.last
    else
      n = line.split(" ")[1].to_i
      x << x.last
      x << x.last + n
    end
  end

  sum = 0
  x.each_with_index do |value, i|
    sum += (i + 1) * value if (i - 19) % 40 == 0
  end
  puts sum
end