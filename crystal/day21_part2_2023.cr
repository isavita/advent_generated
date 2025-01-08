
struct Complex
  property re : Int64
  property im : Int64

  def initialize(@re : Int64, @im : Int64)
  end

  def +(other : Complex)
    Complex.new(re + other.re, im + other.im)
  end

  def -(other : Complex)
    Complex.new(re - other.re, im - other.im)
  end

  def hash
    re.hash ^ im.hash
  end

  def ==(other : Complex)
    re == other.re && im == other.im
  end
end

def complex_mod(num : Complex, mod : Int64) : Complex
  Complex.new((num.re + 10 * mod) % mod, (num.im + 10 * mod) % mod)
end

def parse_data(data : Array(String)) : Tuple(Set(Complex), Complex)
  garden = Set(Complex).new
  start = Complex.new(-1, -1)
  data.each_with_index do |line, y|
    line.each_char_with_index do |c, x|
      if c != '#'
        garden << Complex.new(x.to_i64, y.to_i64)
      end
      if c == 'S'
        start = Complex.new(x.to_i64, y.to_i64)
      end
    end
  end

  if start == Complex.new(-1, -1)
    raise "No start found!"
  end

  {garden, start}
end

def calculate_num_ends(garden : Set(Complex), start : Complex, num_iterations : Int64, max_size : Int64) : Int64
  queue = Set(Complex).new
  queue << start

  done = Array(Int64).new

  (3 * max_size).times do |i|
    if (i % max_size) == (max_size - 1) // 2
      done << queue.size
    end
    if done.size == 3
      break
    end

    new_queue = Set(Complex).new

    [Complex.new(1, 0), Complex.new(-1, 0), Complex.new(0, 1), Complex.new(0, -1)].each do |dir|
      queue.each do |point|
        if garden.includes?(complex_mod(point + dir, max_size))
          new_queue << point + dir
        end
      end
    end
    queue = new_queue
  end

  quadratic_function = ->(n : Int64, a : Int64, b : Int64, c : Int64) {
    a + n * (b - a + ((n - 1) * (c - 2 * b + a) // 2))
  }

  quadratic_function.call(num_iterations // max_size, done[0], done[1], done[2])
end

file = File.open("input.txt")
garden_input = file.each_line.to_a
file.close

garden, start = parse_data(garden_input)
max_size = garden_input.size.to_i64

sum = calculate_num_ends(garden, start, 26501365, max_size)

puts sum
