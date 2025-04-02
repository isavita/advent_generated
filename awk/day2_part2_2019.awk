
#!/usr/bin/awk -f

function run_intcode(noun, verb, program,   i, op, tmp_program, len) {
  split(program, tmp_program, ",")
  tmp_program[2] = noun
  tmp_program[3] = verb
  len = split(program, a, ",")
  i = 1
  while (tmp_program[i] != 99) {
    op = tmp_program[i]
    if (op == 1) {
      tmp_program[tmp_program[i + 3] + 1] = tmp_program[tmp_program[i + 1] + 1] + tmp_program[tmp_program[i + 2] + 1]
    } else if (op == 2) {
      tmp_program[tmp_program[i + 3] + 1] = tmp_program[tmp_program[i + 1] + 1] * tmp_program[tmp_program[i + 2] + 1]
    }
    i += 4
  }
  return tmp_program[1]
}

BEGIN {
  FS=","
  file = "input.txt"
  while (getline < file) {
    program = $0
  }
  close(file)

  # Part One
  result1 = run_intcode(12, 2, program)
  print result1

  # Part Two
  for (noun = 0; noun < 100; noun++) {
    for (verb = 0; verb < 100; verb++) {
      if (run_intcode(noun, verb, program) == 19690720) {
        result2 = 100 * noun + verb
        print result2
        exit
      }
    }
  }
}
