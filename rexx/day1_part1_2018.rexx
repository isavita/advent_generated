
/* REXX */
main:
  frequency = 0
  fileName = 'input.txt'
  DO WHILE lines(fileName) > 0
    frequency = frequency + linein(fileName)
  END
  SAY frequency
  EXIT
