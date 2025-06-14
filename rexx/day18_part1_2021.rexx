
main:
  fname = "input.txt"
  lines.0 = 0
  DO WHILE LINES(fname) > 0
    line = STRIP(LINEIN(fname))
    IF line \= '' THEN DO
      c = lines.0 + 1
      lines.c = line
      lines.0 = c
    END
  END
  CLOSE fname

  current_num = lines.1
  DO i = 2 TO lines.0
    current_num = add(current_num, lines.i)
  END

  SAY magnitude(current_num)
  EXIT

add: PROCEDURE
  ARG num1, num2
  RETURN reduce('[' || num1 || ',' || num2 || ']')

reduce: PROCEDURE
  ARG num
  DO FOREVER
    prev_num = num
    num = explode(num)
    IF num \= prev_num THEN ITERATE

    num = split(num)
    IF num \= prev_num THEN ITERATE

    LEAVE
  END
  RETURN num

explode: PROCEDURE
  ARG num
  depth = 0
  DO i = 1 TO LENGTH(num)
    char = SUBSTR(num, i, 1)
    IF char = '[' THEN depth = depth + 1
    ELSE IF char = ']' THEN depth = depth - 1

    IF depth < 5 THEN ITERATE

    start_explode = i
    end_explode = POS(']', num, start_explode)
    pair_str = SUBSTR(num, start_explode + 1, end_explode - start_explode - 1)
    PARSE VAR pair_str left_val ',' right_val

    prefix = SUBSTR(num, 1, start_explode - 1)
    suffix = SUBSTR(num, end_explode + 1)

    new_prefix = add_to_last_num(prefix, left_val)
    new_suffix = add_to_first_num(suffix, right_val)

    RETURN new_prefix || '0' || new_suffix
  END
  RETURN num

add_to_last_num: PROCEDURE
  ARG str, val
  num_end = 0
  DO i = LENGTH(str) TO 1 BY -1
    IF DATATYPE(SUBSTR(str, i, 1), 'N') THEN DO; num_end = i; LEAVE; END
  END
  IF num_end = 0 THEN RETURN str

  num_start = num_end
  DO i = num_end - 1 TO 1 BY -1
    IF \DATATYPE(SUBSTR(str, i, 1), 'N') THEN LEAVE
    num_start = i
  END

  old_num = SUBSTR(str, num_start, num_end - num_start + 1)
  new_num = old_num + val
  RETURN SUBSTR(str, 1, num_start - 1) || new_num || SUBSTR(str, num_end + 1)

add_to_first_num: PROCEDURE
  ARG str, val
  num_start = 0
  DO i = 1 TO LENGTH(str)
    IF DATATYPE(SUBSTR(str, i, 1), 'N') THEN DO; num_start = i; LEAVE; END
  END
  IF num_start = 0 THEN RETURN str

  num_end = num_start
  DO i = num_start + 1 TO LENGTH(str)
    IF \DATATYPE(SUBSTR(str, i, 1), 'N') THEN LEAVE
    num_end = i
  END

  old_num = SUBSTR(str, num_start, num_end - num_start + 1)
  new_num = old_num + val
  RETURN SUBSTR(str, 1, num_start - 1) || new_num || SUBSTR(str, num_end + 1)

split: PROCEDURE
  ARG num
  i = 1
  DO WHILE i <= LENGTH(num)
    IF \DATATYPE(SUBSTR(num, i, 1), 'N') THEN DO; i = i + 1; ITERATE; END

    num_start = i
    num_end = i
    DO j = i + 1 TO LENGTH(num)
      IF \DATATYPE(SUBSTR(num, j, 1), 'N') THEN LEAVE
      num_end = j
    END

    val = SUBSTR(num, num_start, num_end - num_start + 1)
    IF val >= 10 THEN DO
      left = val % 2
      right = val - left
      new_pair = '[' || left || ',' || right || ']'
      RETURN SUBSTR(num, 1, num_start - 1) || new_pair || SUBSTR(num, num_end + 1)
    END
    i = num_end + 1
  END
  RETURN num

magnitude: PROCEDURE
  ARG num
  DO WHILE POS('[', num) > 0
    best_start = 0
    p = POS('[', num)
    DO WHILE p > 0
      next_open = POS('[', num, p + 1)
      next_close = POS(']', num, p + 1)
      IF next_open = 0 | next_close < next_open THEN
        best_start = p
      p = POS('[', num, p + 1)
    END

    start_pos = best_start
    end_pos = POS(']', num, start_pos)
    pair_content = SUBSTR(num, start_pos + 1, end_pos - start_pos - 1)
    PARSE VAR pair_content left_val ',' right_val

    mag = 3 * left_val + 2 * right_val
    num = SUBSTR(num, 1, start_pos - 1) || mag || SUBSTR(num, end_pos + 1)
  END
  RETURN num
