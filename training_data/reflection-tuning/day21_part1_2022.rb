# Read and parse the input file
monkeys = {}
File.readlines('input.txt').each do |line|
  name, job = line.strip.split(': ')
  monkeys[name] = job.match?(/\d+/) ? job.to_i : job.split
end

# Memoization cache
@cache = {}

# Recursive function to evaluate a monkey's job
def evaluate(monkey, monkeys)
  return @cache[monkey] if @cache.key?(monkey)

  job = monkeys[monkey]
  result = if job.is_a?(Integer)
    job
  else
    left = evaluate(job[0], monkeys)
    right = evaluate(job[2], monkeys)
    case job[1]
    when '+' then left + right
    when '-' then left - right
    when '*' then left * right
    when '/' then left / right
    end
  end

  @cache[monkey] = result
end

# Evaluate the root monkey and print the result
puts evaluate('root', monkeys)
