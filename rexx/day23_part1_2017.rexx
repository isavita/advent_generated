
/* REXX */
call main
exit

main:
  reg. = 0
  mulCount = 0
  ip = 1
  call readInstructions 'input.txt'

  do while ip > 0 & ip <= instructions.0
    parse var instructions.ip cmd x y

    select
      when cmd = 'set' then
        reg.x = getValue(y)
      when cmd = 'sub' then
        reg.x = reg.x - getValue(y)
      when cmd = 'mul' then do
        reg.x = reg.x * getValue(y)
        mulCount = mulCount + 1
      end
      when cmd = 'jnz' then do
        if getValue(x) \= 0 then
          ip = ip + getValue(y) - 1
      end
    end
    ip = ip + 1
  end

  say mulCount
return

getValue: procedure expose reg.
  parse arg val
  if datatype(val, 'W') then return val
  return reg.val

readInstructions: procedure expose instructions.
  parse arg fileName
  i = 0
  do while lines(fileName) > 0
    i = i + 1
    instructions.i = linein(fileName)
  end
  instructions.0 = i
  call stream fileName, 'C', 'CLOSE'
return
