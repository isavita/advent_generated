
/* REXX */
count = 0
fn = 'input.txt'
prev = linein(fn)
do while lines(fn) > 0
  current = linein(fn)
  if current > prev then
    count = count + 1
  prev = current
end
say count
