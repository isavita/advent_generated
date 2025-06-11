
/* REXX */
call main
exit

main:
  call readInput
  stabilized = 0
  do until stabilized
    call simulateSeating
  end
  say countOccupiedSeats()
return

readInput:
  procedure expose seating. rows cols
  file = 'input.txt'
  rows = 0
  do while lines(file) > 0
    rows = rows + 1
    seating.rows = linein(file)
  end
  call stream file, 'c', 'close'
  if rows > 0 then cols = length(seating.1)
  else cols = 0
return

simulateSeating:
  procedure expose seating. rows cols stabilized
  changed = 0
  do i = 1 to rows
    new_seating.i = seating.i
  end

  do r = 1 to rows
    do c = 1 to cols
      seat = substr(seating.r, c, 1)
      if seat = '.' then iterate

      adjacent = countAdjacent(r, c)

      if seat = 'L' & adjacent = 0 then do
        new_seating.r = overlay('#', new_seating.r, c)
        changed = 1
      end
      else if seat = '#' & adjacent >= 4 then do
        new_seating.r = overlay('L', new_seating.r, c)
        changed = 1
      end
    end
  end

  if changed then do
    do i = 1 to rows
      seating.i = new_seating.i
    end
  end
  else stabilized = 1
return

countAdjacent:
  procedure expose seating. rows cols
  parse arg r, c
  count = 0
  do i = r - 1 to r + 1
    do j = c - 1 to c + 1
      if i = r & j = c then iterate
      if i > 0 & i <= rows & j > 0 & j <= cols then
        if substr(seating.i, j, 1) = '#' then
          count = count + 1
    end
  end
return count

countOccupiedSeats:
  procedure expose seating. rows
  total = 0
  do i = 1 to rows
    total = total + countstr('#', seating.i)
  end
return total
