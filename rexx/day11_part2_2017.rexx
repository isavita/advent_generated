
/* REXX */
call main
exit

main:
  x = 0
  y = 0
  z = 0
  max_dist = 0
  
  path = linein('input.txt')
  
  do while path <> ''
    parse var path step ',' path
    
    select
      when step = 'n'  then do; y = y + 1; z = z - 1; end
      when step = 'ne' then do; x = x + 1; z = z - 1; end
      when step = 'se' then do; x = x + 1; y = y - 1; end
      when step = 's'  then do; y = y - 1; z = z + 1; end
      when step = 'sw' then do; x = x - 1; z = z + 1; end
      when step = 'nw' then do; x = x - 1; y = y + 1; end
    end
    
    max_dist = max(max_dist, (abs(x) + abs(y) + abs(z)) % 2)
  end
  
  say max_dist
return
