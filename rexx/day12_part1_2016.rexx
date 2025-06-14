
main:
  reg. = 0
  file = 'input.txt'
  i = 0
  DO WHILE lines(file) > 0
    i = i + 1
    instr.i = linein(file)
  END
  instr.0 = i
  CALL lineout file

  ip = 1
  DO WHILE ip > 0 & ip <= instr.0
    PARSE VAR instr.ip opcode arg1 arg2 .
    jump = 1

    SELECT
      WHEN opcode = 'cpy' THEN CALL value 'reg.'arg2, getValue(arg1)
      WHEN opcode = 'inc' THEN CALL value 'reg.'arg1, value('reg.'arg1) + 1
      WHEN opcode = 'dec' THEN CALL value 'reg.'arg1, value('reg.'arg1) - 1
      WHEN opcode = 'jnz' THEN IF getValue(arg1) <> 0 THEN jump = arg2
    END
    ip = ip + jump
  END

  SAY reg.a
  EXIT

getValue:
  PARSE ARG operand
  IF datatype(operand, 'N') THEN RETURN operand
  ELSE RETURN value('reg.'operand)
