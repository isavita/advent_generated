
/* Rexx */
call main
exit

main:
  call stream 'input.txt', 'c', 'open read'
  pipes. = ''
  do forever
    line = linein('input.txt')
    if stream('input.txt', 's') \= 'READY' then leave
    parse var line program '<->' connections
    program = strip(program)
    connections = space(translate(connections, ' ', ','), 1)
    pipes.program = connections
  end
  call stream 'input.txt', 'c', 'close'

  group. = 0
  group_count = 0
  call find_group 0

  say group_count
return

find_group: procedure expose pipes. group. group_count
  parse arg current_program
  if group.current_program then return

  group.current_program = 1
  group_count = group_count + 1

  neighbors = pipes.current_program
  do i = 1 to words(neighbors)
    call find_group word(neighbors, i)
  end
return
