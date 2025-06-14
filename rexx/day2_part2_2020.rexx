
main:
  validCount1 = 0
  validCount2 = 0
  fileName = 'input.txt'

  DO WHILE LINES(fileName) > 0
    line = LINEIN(fileName)
    PARSE VAR line min '-' max ' ' letter ':' password
    password = STRIP(password)

    count = COUNTSTR(letter, password)
    IF count >= min & count <= max THEN
      validCount1 = validCount1 + 1

    pos1_match = (SUBSTR(password, min, 1) = letter)
    pos2_match = (SUBSTR(password, max, 1) = letter)
    IF pos1_match \= pos2_match THEN
      validCount2 = validCount2 + 1
  END

  SAY validCount1
  SAY validCount2
RETURN
