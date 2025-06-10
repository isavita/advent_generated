
/* Rexx */
call main
exit

main:
  password = 'abcdefgh'
  filename = 'input.txt'

  do while lines(filename) > 0
    op = linein(filename)
    cmd = word(op, 1)
    p1 = word(op, 2)

    select
      when cmd = 'swap' then do
        if p1 = 'position' then do
          x = word(op, 3) + 1
          y = word(op, 6) + 1
          char_x = substr(password, x, 1)
          char_y = substr(password, y, 1)
          password = overlay(char_y, password, x)
          password = overlay(char_x, password, y)
        end
        else do
          x = word(op, 3)
          y = word(op, 6)
          password = translate(password, x || y, y || x)
        end
      end
      when cmd = 'rotate' then do
        len = length(password)
        if p1 = 'left' then do
          steps = word(op, 3) // len
          password = substr(password, steps + 1) || left(password, steps)
        end
        else if p1 = 'right' then do
          steps = word(op, 3) // len
          password = right(password, steps) || substr(password, 1, len - steps)
        end
        else do
          letter = word(op, 7)
          idx = pos(letter, password)
          steps = idx
          if idx >= 5 then steps = steps + 1
          steps = steps // len
          password = right(password, steps) || substr(password, 1, len - steps)
        end
      end
      when cmd = 'reverse' then do
        x = word(op, 3) + 1
        y = word(op, 5) + 1
        if x > y then parse value x y with y x
        prefix = left(password, x - 1)
        middle = substr(password, x, y - x + 1)
        suffix = substr(password, y + 1)
        password = prefix || reverse(middle) || suffix
      end
      when cmd = 'move' then do
        x = word(op, 3) + 1
        y = word(op, 6) + 1
        char = substr(password, x, 1)
        password = delstr(password, x, 1)
        password = insert(char, password, y - 1)
      end
    end
  end
  close(filename)

  say password
return
