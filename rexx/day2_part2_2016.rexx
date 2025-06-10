
/* Rexx */
call main
exit

main:
  call setup
  call solve 'keypad1.', 3, 2, 2
  call solve 'keypad2.', 5, 3, 1
  return

setup:
  keypad1. = 0
  keypad1.1.1 = 1; keypad1.1.2 = 2; keypad1.1.3 = 3
  keypad1.2.1 = 4; keypad1.2.2 = 5; keypad1.2.3 = 6
  keypad1.3.1 = 7; keypad1.3.2 = 8; keypad1.3.3 = 9

  keypad2. = 0
  keypad2.1.3 = 1
  keypad2.2.2 = 2; keypad2.2.3 = 3; keypad2.2.4 = 4
  keypad2.3.1 = 5; keypad2.3.2 = 6; keypad2.3.3 = 7; keypad2.3.4 = 8; keypad2.3.5 = 9
  keypad2.4.2 = 'A'; keypad2.4.3 = 'B'; keypad2.4.4 = 'C'
  keypad2.5.3 = 'D'

  instructions.0 = 0
  do i = 1 while lines('input.txt') > 0
    instructions.i = linein('input.txt')
    instructions.0 = i
  end
  call lineout 'input.txt'
  return

solve:
  procedure expose keypad1. keypad2. instructions.
  parse arg keypad_stem, dim, x, y
  code = ''
  do i = 1 to instructions.0
    line = instructions.i
    do j = 1 to length(line)
      move = substr(line, j, 1)
      nx = x; ny = y
      select
        when move = 'U' then nx = x - 1
        when move = 'D' then nx = x + 1
        when move = 'L' then ny = y - 1
        when move = 'R' then ny = y + 1
        otherwise nop
      end
      if nx > 0 & nx <= dim & ny > 0 & ny <= dim then do
        if value(keypad_stem || nx || '.' || ny) \= 0 then do
          x = nx; y = ny
        end
      end
    end
    code = code || value(keypad_stem || x || '.' || y)
  end
  say code
  return
