input = linein("input.txt")
floor = 0
do i = 1 to length(input)
   c = substr(input, i, 1)
   if c = '(' then floor = floor + 1
   else if c = ')' then floor = floor - 1
end
say floor