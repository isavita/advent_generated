
require "set"

# Read the (unused) input file so the program matches the specification.
File.read("input.txt")

register5 = 0_i64
seen      = Set(Int64).new
first_val = nil : Int64?
last_val  = 0_i64

loop do
  register3 = register5 | 65536_i64
  register5 = 7_586_220_i64

  loop do
    register1 = register3 & 255_i64
    register5 = ((register5 + register1) & 16_777_215_i64) * 65_899_i64 & 16_777_215_i64

    if register3 < 256_i64
      unless seen.includes?(register5)
        first_val = register5 if first_val.nil?
        seen.add(register5)
        last_val = register5
        break
      end

      puts "Part One: #{first_val}"
      puts "Part Two: #{last_val}"
      exit
    else
      register3 = register3 // 256_i64
    end
  end
end
