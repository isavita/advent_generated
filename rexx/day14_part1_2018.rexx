
main:
  input = LINEIN('input.txt')

  scoreboard.0 = 3
  scoreboard.1 = 7
  num_recipes = 2
  elf1 = 0
  elf2 = 1

  DO WHILE num_recipes < input + 10
    sum = scoreboard.elf1 + scoreboard.elf2
    IF sum >= 10 THEN DO
      scoreboard.num_recipes = 1
      num_recipes = num_recipes + 1
    END
    scoreboard.num_recipes = sum // 10
    num_recipes = num_recipes + 1

    elf1 = (elf1 + 1 + scoreboard.elf1) // num_recipes
    elf2 = (elf2 + 1 + scoreboard.elf2) // num_recipes
  END

  result = ''
  DO i = input TO input + 9
    result = result || scoreboard.i
  END
  SAY result

RETURN
