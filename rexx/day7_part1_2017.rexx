
/* Rexx solution */
call main
exit

main:
  fn = 'input.txt'
  holder_list = ''
  held. = 0

  do while lines(fn) > 0
    line = linein(fn)
    line = translate(line, ' ', ',')

    holder_list = holder_list word(line, 1)

    do i = 2 to words(line)
      held.word(line, i) = 1
    end
  end
  call stream fn, 'c', 'close'

  do name in holder_list
    if held.name = 0 then do
      say name
    end
  end
return
