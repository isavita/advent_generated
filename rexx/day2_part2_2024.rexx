
/* Rexx */
main:
  safe_count = 0
  filename = 'input.txt'
  DO WHILE LINES(filename) > 0
    line = LINEIN(filename)
    IF IsSafe(line) THEN
      safe_count = safe_count + 1
  END
  CALL LINEOUT filename
  SAY safe_count
EXIT

IsSafe:
  PROCEDURE
  PARSE ARG report
  IF IsSafeSequence(report) THEN RETURN 1

  count = WORDS(report)
  DO i = 1 TO count
    before = ''
    IF i > 1 THEN before = SUBWORD(report, 1, i - 1)
    after = ''
    IF i < count THEN after = SUBWORD(report, i + 1)
    modified_report = SPACE(before after, 1)
    IF IsSafeSequence(modified_report) THEN RETURN 1
  END
  RETURN 0

IsSafeSequence:
  PROCEDURE
  PARSE ARG levels
  count = WORDS(levels)
  IF count < 2 THEN RETURN 1

  prev = WORD(levels, 1)
  curr = WORD(levels, 2)
  diff = curr - prev
  IF diff = 0 THEN RETURN 0
  trend = SIGN(diff)
  abs_diff = ABS(diff)
  IF abs_diff < 1 | abs_diff > 3 THEN RETURN 0

  DO i = 3 TO count
    prev = curr
    curr = WORD(levels, i)
    diff = curr - prev
    IF diff = 0 THEN RETURN 0
    IF SIGN(diff) \= trend THEN RETURN 0
    abs_diff = ABS(diff)
    IF abs_diff < 1 | abs_diff > 3 THEN RETURN 0
  END
  RETURN 1
