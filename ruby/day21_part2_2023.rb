
def check(e)
  raise e if e
end

def parse_data(data)
  garden = {}
  start = nil
  data.each_with_index do |line, y|
    line.chars.each_with_index do |c, x|
      garden[Complex(x, y)] = true unless c == '#'
      start = Complex(x, y) if c == 'S'
    end
  end
  raise "No start found!" unless start
  [garden, start]
end

def complex_mod(num, mod)
  raise "Complex number not integer!" unless num.real.to_i == num.real && num.imag.to_i == num.imag
  Complex((num.real.to_i + 10 * mod) % mod, (num.imag.to_i + 10 * mod) % mod)
end

def calculate_num_ends(garden, start, num_iterations, max_size)
  queue = { start => true }
  done = []

  (0...3 * max_size).each do |i|
    if (i % max_size) == (max_size - 1) / 2
      done << queue.size
    end
    break if done.size == 3

    new_queue = {}
    [Complex(1, 0), Complex(-1, 0), Complex(0, 1), Complex(0, -1)].each do |dir|
      queue.each_key do |point|
        new_point = point + dir
        new_queue[new_point] = true if garden[complex_mod(new_point, max_size)]
      end
    end
    queue = new_queue
  end

  quadratic_function = ->(n, a, b, c) {
    a + n * (b - a + ((n - 1) * (c - 2 * b + a) / 2))
  }

  quadratic_function.call(num_iterations / max_size, done[0], done[1], done[2])
end

begin
  file_path = 'input.txt'
  garden_input = File.readlines(file_path, chomp: true)

  garden, start = parse_data(garden_input)
  max_size = garden_input.size

  sum = calculate_num_ends(garden, start, 26501365, max_size)
  puts sum
rescue StandardError => e
  puts "Error: #{e}"
end
