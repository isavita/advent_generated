
/* REXX */
main:
  caves. = 0
  count = 0
  filename = 'input.txt'

  do while lines(filename) > 0
    line = linein(filename)
    parse var line from '-' to

    i = caves.from.0 + 1
    caves.from.i = to
    caves.from.0 = i

    j = caves.to.0 + 1
    caves.to.j = from
    caves.to.0 = j
  end

  call dfs 'start', 'start'

  say count
exit

dfs:
  procedure expose caves. count
  parse arg current, visited

  if current = 'end' then do
    count = count + 1
    return
  end

  do i = 1 to caves.current.0
    next_cave = caves.current.i
    if lower(next_cave) = next_cave & wordpos(next_cave, visited) > 0 then
      iterate

    call dfs next_cave, visited next_cave
  end
return
