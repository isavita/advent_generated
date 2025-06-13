
/* Rexx */
call main
exit

main:
  total_rows = 40
  current_row = linein('input.txt')
  row_len = length(current_row)
  safe_tiles = countstr('.', current_row)

  do total_rows - 1
    prev_row = '.' || current_row || '.'
    next_row = ''
    do j = 1 to row_len
      if substr(prev_row, j, 1) \= substr(prev_row, j + 2, 1) then
        next_row = next_row || '^'
      else
        next_row = next_row || '.'
    end
    safe_tiles = safe_tiles + countstr('.', next_row)
    current_row = next_row
  end

  say safe_tiles
return
