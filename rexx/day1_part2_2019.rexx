
/* REXX */
total_fuel = 0
fileName = 'input.txt'

DO WHILE LINES(fileName) > 0
  mass = LINEIN(fileName)
  total_fuel = total_fuel + calculate_fuel(mass)
END

SAY total_fuel
EXIT

calculate_fuel: PROCEDURE
  ARG mass
  fuel = mass % 3 - 2
  IF fuel <= 0 THEN
    RETURN 0
  RETURN fuel + calculate_fuel(fuel)
