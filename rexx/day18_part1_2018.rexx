
/* Rexx */
call main
exit

main:
  SIZE = 50
  OPEN = '.'
  TREES = '|'
  LUMBERYARD = '#'
  FILENAME = 'input.txt'
  grid. = ''

  do i = 1 to SIZE
    line = linein(FILENAME)
    do j = 1 to SIZE
      grid.i.j = substr(line, j, 1)
    end
  end
  call stream FILENAME, 'c', 'close'

  do minute = 1 to 10
    call transform
  end

  parse value count_resources() with wooded lumberyards
  say wooded * lumberyards
return

transform: procedure expose grid. SIZE OPEN TREES LUMBERYARD
  new_grid. = ''
  do i = 1 to SIZE
    do j = 1 to SIZE
      new_grid.i.j = next_acre_state(i, j)
    end
  end
  do i = 1 to SIZE
    do j = 1 to SIZE
      grid.i.j = new_grid.i.j
    end
  end
return

next_acre_state: procedure expose grid. SIZE OPEN TREES LUMBERYARD
  parse arg i, j
  current_state = grid.i.j
  select
    when current_state = OPEN then do
      if count_adjacent(i, j, TREES) >= 3 then return TREES
    end
    when current_state = TREES then do
      if count_adjacent(i, j, LUMBERYARD) >= 3 then return LUMBERYARD
    end
    when current_state = LUMBERYARD then do
      if count_adjacent(i, j, LUMBERYARD) >= 1 & ,
         count_adjacent(i, j, TREES) >= 1 then
        return LUMBERYARD
      else
        return OPEN
    end
  end
  return current_state

count_adjacent: procedure expose grid. SIZE
  parse arg i, j, acre_type
  count = 0
  do x = -1 to 1
    do y = -1 to 1
      if x = 0 & y = 0 then iterate
      ni = i + x
      nj = j + y
      if ni >= 1 & ni <= SIZE & nj >= 1 & nj <= SIZE then do
        if grid.ni.nj = acre_type then
          count = count + 1
      end
    end
  end
  return count

count_resources: procedure expose grid. SIZE TREES LUMBERYARD
  wooded = 0
  lumberyards = 0
  do i = 1 to SIZE
    do j = 1 to SIZE
      select
        when grid.i.j = TREES then wooded = wooded + 1
        when grid.i.j = LUMBERYARD then lumberyards = lumberyards + 1
        otherwise nop
      end
    end
  end
  return wooded lumberyards
