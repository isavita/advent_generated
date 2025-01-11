
def instructions
  {
    "addr" => ->(r : Array(Int32), a : Int32, b : Int32) { r[a] + r[b] },
    "addi" => ->(r : Array(Int32), a : Int32, b : Int32) { r[a] + b },
    "mulr" => ->(r : Array(Int32), a : Int32, b : Int32) { r[a] * r[b] },
    "muli" => ->(r : Array(Int32), a : Int32, b : Int32) { r[a] * b },
    "banr" => ->(r : Array(Int32), a : Int32, b : Int32) { r[a] & r[b] },
    "bani" => ->(r : Array(Int32), a : Int32, b : Int32) { r[a] & b },
    "borr" => ->(r : Array(Int32), a : Int32, b : Int32) { r[a] | r[b] },
    "bori" => ->(r : Array(Int32), a : Int32, b : Int32) { r[a] | b },
    "setr" => ->(r : Array(Int32), a : Int32, b : Int32) { r[a] },
    "seti" => ->(r : Array(Int32), a : Int32, b : Int32) { a },
    "gtir" => ->(r : Array(Int32), a : Int32, b : Int32) { a > r[b] ? 1 : 0 },
    "gtri" => ->(r : Array(Int32), a : Int32, b : Int32) { r[a] > b ? 1 : 0 },
    "gtrr" => ->(r : Array(Int32), a : Int32, b : Int32) { r[a] > r[b] ? 1 : 0 },
    "eqir" => ->(r : Array(Int32), a : Int32, b : Int32) { a == r[b] ? 1 : 0 },
    "eqri" => ->(r : Array(Int32), a : Int32, b : Int32) { r[a] == b ? 1 : 0 },
    "eqrr" => ->(r : Array(Int32), a : Int32, b : Int32) { r[a] == r[b] ? 1 : 0 },
  }
end

def load_program(lines)
  program = [] of Proc(Array(Int32), Nil)
  ip_register = 0
  lines.each do |line|
    if line.starts_with?("#ip")
      ip_register = line.split[1].to_i
      next
    end
    parts = line.split
    op = instructions[parts[0]]
    nums = parts[1..3].map(&.to_i)
    a, b, c = nums
    program << ->(r : Array(Int32)) { r[c] = op.call(r, a, b) }
  end
  {ip_register, program}
end

def run_program(ip_register, program, registers, max_cycles)
  ip = 0
  cycles = 0
  while ip >= 0 && ip < program.size
    registers[ip_register] = ip
    program[ip].call(registers)
    ip = registers[ip_register] + 1
    cycles += 1
    break if max_cycles > 0 && cycles >= max_cycles
  end
  registers
end

lines = File.read_lines("input.txt").reject(&.empty?)
ip_register, program = load_program(lines)
registers = [1, 0, 0, 0, 0, 0].map(&.to_i32)
registers = run_program(ip_register, program, registers, 1000)
n = registers.max
total = 0
(1..Math.sqrt(n).to_i).each do |i|
  if n % i == 0
    total += i
    total += n // i if i * i != n
  end
end
puts total
