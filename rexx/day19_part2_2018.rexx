
/* REXX */
main:
  do i = 0 to 5; r.i = 0; end
  r.0 = 1

  fileName = 'input.txt'
  instrCount = 0
  do while LINES(fileName) > 0
    line = LINEIN(fileName)
    if line = '' then iterate
    if SUBSTR(line, 1, 3) = '#ip' then
      parse var line . ipRegister
    else do
      program.instrCount = line
      instrCount = instrCount + 1
    end
  end
  call LINEIN fileName, 1, 0

  ip = 0
  do cycles = 1 to 1000
    if ip < 0 | ip >= instrCount then leave
    r.ipRegister = ip
    parse var program.ip op a b c

    select
      when op = 'addr' then r.c = r.a + r.b
      when op = 'addi' then r.c = r.a + b
      when op = 'mulr' then r.c = r.a * r.b
      when op = 'muli' then r.c = r.a * b
      when op = 'banr' then r.c = BITAND(r.a, r.b)
      when op = 'bani' then r.c = BITAND(r.a, b)
      when op = 'borr' then r.c = BITOR(r.a, r.b)
      when op = 'bori' then r.c = BITOR(r.a, b)
      when op = 'setr' then r.c = r.a
      when op = 'seti' then r.c = a
      when op = 'gtir' then if a > r.b then r.c = 1; else r.c = 0
      when op = 'gtri' then if r.a > b then r.c = 1; else r.c = 0
      when op = 'gtrr' then if r.a > r.b then r.c = 1; else r.c = 0
      when op = 'eqir' then if a = r.b then r.c = 1; else r.c = 0
      when op = 'eqri' then if r.a = b then r.c = 1; else r.c = 0
      when op = 'eqrr' then if r.a = r.b then r.c = 1; else r.c = 0
    end

    ip = r.ipRegister + 1
  end

  n = 0
  do i = 0 to 5
    if r.i > n then n = r.i
  end

  total = 0
  do i = 1 to n
    if n // i = 0 then total = total + i
  end

  say total
return
