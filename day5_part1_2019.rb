
input = File.read("input.txt").strip.split(",").map(&:to_i)

def run_program(input)
  i = 0
  while i < input.length
    opcode = input[i] % 100
    modes = input[i] / 100

    case opcode
    when 1
      val1 = input[i + 1]
      val1 = input[val1] if modes % 10 == 0
      modes /= 10
      val2 = input[i + 2]
      val2 = input[val2] if modes % 10 == 0
      modes /= 10
      val3 = input[i + 3]
      input[val3] = val1 + val2
      i += 4
    when 2
      val1 = input[i + 1]
      val1 = input[val1] if modes % 10 == 0
      modes /= 10
      val2 = input[i + 2]
      val2 = input[val2] if modes % 10 == 0
      modes /= 10
      val3 = input[i + 3]
      input[val3] = val1 * val2
      i += 4
    when 3
      print "Input ID: 1\n"
      input[input[i + 1]] = 1
      i += 2
    when 4
      val = input[i + 1]
      val = input[val] if modes % 10 == 0
      print "Diagnostic code: #{val}\n"
      i += 2
    when 99
      break
    end
  end
end

run_program(input)
