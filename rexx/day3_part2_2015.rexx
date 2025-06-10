
/* Rexx */
call main
exit

main:
  houses. = 0
  x.0 = 0; y.0 = 0
  x.1 = 0; y.1 = 0

  houses.0.0 = 1
  count = 1

  directions = linein('input.txt')

  do i = 1 to length(directions)
    turn = i // 2
    char = substr(directions, i, 1)

    select
      when char = '^' then y.turn = y.turn + 1
      when char = 'v' then y.turn = y.turn - 1
      when char = '>' then x.turn = x.turn + 1
      when char = '<' then x.turn = x.turn - 1
    end

    key = x.turn'.'y.turn
    if houses.key = 0 then do
      houses.key = 1
      count = count + 1
    end
  end

  say count
return
