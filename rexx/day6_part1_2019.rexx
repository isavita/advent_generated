
/* REXX */
main:
  orbitMap. = ''
  fileName = 'input.txt'

  DO WHILE lines(fileName) > 0
    line = linein(fileName)
    PARSE VAR line center ')' orbiter
    orbitMap.center = strip(orbitMap.center orbiter)
  END
  CALL lineout fileName

  SAY countOrbits('COM', 0)
EXIT

countOrbits:
  PROCEDURE EXPOSE orbitMap.
  PARSE ARG node, depth
  total = depth
  orbiters = orbitMap.node
  DO i = 1 TO words(orbiters)
    total = total + countOrbits(word(orbiters, i), depth + 1)
  END
  RETURN total
