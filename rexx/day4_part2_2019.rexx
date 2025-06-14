
/* Rexx */
line = STREAM('input.txt', 'c', 'QUERY EXISTS')
IF line = '' THEN EXIT
line = LINEIN('input.txt')
CALL LINEOUT 'input.txt'
PARSE VAR line lower_bound '-' upper_bound

count = 0
DO i = lower_bound TO upper_bound
  IF check_criteria(i) THEN
    count = count + 1
END

SAY count
EXIT

check_criteria:
  PROCEDURE
  s = ARG(1)
  len = LENGTH(s)
  has_pair_of_two = 0

  DO j = 2 TO len
    IF SUBSTR(s, j, 1) < SUBSTR(s, j - 1, 1) THEN
      RETURN 0
  END

  k = 1
  DO WHILE k <= len
    digit = SUBSTR(s, k, 1)
    run_len = 1
    DO p = k + 1 TO len
      IF SUBSTR(s, p, 1) \= digit THEN LEAVE
      run_len = run_len + 1
    END
    IF run_len = 2 THEN
      has_pair_of_two = 1
    k = k + run_len
  END

  RETURN has_pair_of_two
