file = File.open("input.txt")
jobs = {} of String => String
results = {} of String => Int64

file.each_line do |line|
  parts = line.chomp.split(": ")
  jobs[parts[0]] = parts[1]
end

puts calculate("root", jobs, results)

def calculate(monkey, jobs, results)
  if results.has_key?(monkey)
    return results[monkey]
  end

  job = jobs[monkey]
  if job.nil?
    raise "Monkey not found: #{monkey}"
  end

  if num = job.to_i64?
    results[monkey] = num
    return num
  end

  parts = job.split
  a = calculate(parts[0], jobs, results)
  b = calculate(parts[2], jobs, results)

  case parts[1]
  when "+"
    results[monkey] = a + b
  when "-"
    results[monkey] = a - b
  when "*"
    results[monkey] = a * b
  when "/"
    if b == 0
      raise "Division by zero"
    end
    results[monkey] = a // b
  else
    raise "Unknown operation: #{parts[1]}"
  end
  results[monkey]
end