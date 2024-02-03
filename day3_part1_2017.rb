
input = File.read('input.txt').to_i

def spiral_memory_steps(input)
  n = Math.sqrt(input).ceil
  n += 1 if n % 2 == 0
  steps = (n - 1) / 2
  dist = (n ** 2 - input) % (n - 1)
  dist = n - 1 if dist == 0
  steps + (dist - steps).abs
end

puts spiral_memory_steps(input)
