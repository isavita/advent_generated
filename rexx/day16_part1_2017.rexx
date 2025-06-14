
/* REXX */
main:
  programs = 'abcdefghijklmnop'
  n = length(programs)

  call stream 'input.txt', 'c', 'open read'
  line = linein('input.txt')
  call stream 'input.txt', 'c', 'close'

  do while line <> ''
    parse var line move ',' line
    if move = '' then iterate

    type = left(move, 1)
    params = substr(move, 2)

    select
      when type = 's' then do
        x = params
        programs = right(programs, x) || left(programs, n - x)
      end

      when type = 'x' then do
        parse var params a '/' b
        a = a + 1
        b = b + 1
        char_a = substr(programs, a, 1)
        char_b = substr(programs, b, 1)
        programs = overlay(char_b, programs, a)
        programs = overlay(char_a, programs, b)
      end

      when type = 'p' then do
        parse var params prog_a '/' prog_b
        programs = translate(programs, prog_a || prog_b, prog_b || prog_a)
      end
    end
  end

  say programs
return
