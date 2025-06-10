
/* REXX */
data = LINEIN('input.txt')
disk_len = 272

DO WHILE LENGTH(data) < disk_len
  data = dragon_curve(data)
END

data = SUBSTR(data, 1, disk_len)
SAY checksum(data)
EXIT

dragon_curve: PROCEDURE
  PARSE ARG a
  b = TRANSLATE(REVERSE(a), '01', '10')
  RETURN a || '0' || b

checksum: PROCEDURE
  PARSE ARG c
  DO WHILE LENGTH(c) // 2 = 0
    next_c = ''
    DO i = 1 TO LENGTH(c) BY 2
      IF SUBSTR(c, i, 1) = SUBSTR(c, i + 1, 1) THEN
        next_c = next_c || '1'
      ELSE
        next_c = next_c || '0'
    END
    c = next_c
  END
  RETURN c
