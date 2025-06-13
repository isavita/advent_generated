
/* REXX */
main:
  data = STRIP(LINEIN('input.txt'))
  total = 0
  len = LENGTH(data)
  offset = len % 2

  DO i = 1 TO len
    j = i + offset
    IF j > len THEN
      j = j - len

    IF SUBSTR(data, i, 1) = SUBSTR(data, j, 1) THEN
      total = total + SUBSTR(data, i, 1)
  END

  SAY total
EXIT
