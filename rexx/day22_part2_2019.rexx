
main:
  numeric digits 50

  SIZE = 119315717514047
  ITERATIONS = 101741582076661
  TARGET_POS = 2020
  FILENAME = 'input.txt'

  offset = 0
  increment = 1

  do while lines(FILENAME) > 0
    line = linein(FILENAME)
    select
      when line = 'deal into new stack' then do
        increment = mod(increment * -1, SIZE)
        offset = mod(offset + increment, SIZE)
      end
      when substr(line, 1, 3) = 'cut' then do
        parse var line 'cut' n .
        offset = mod(offset + n * increment, SIZE)
      end
      when substr(line, 1, 19) = 'deal with increment' then do
        parse var line 'deal with increment' n .
        inv = mod_inverse(n, SIZE)
        increment = mod(increment * inv, SIZE)
      end
    end
  end
  call lineout FILENAME

  final_increment = mod_pow(increment, ITERATIONS, SIZE)

  term1 = mod(final_increment - 1, SIZE)
  term2_inv = mod_inverse(mod(increment - 1, SIZE), SIZE)
  final_offset = mod(offset * term1 * term2_inv, SIZE)

  answer = mod(TARGET_POS * final_increment + final_offset, SIZE)

  say answer
exit

mod: procedure
  parse arg n, m
  return (n // m + m) // m

mod_inverse: procedure
  parse arg n, modulus
  return mod_pow(n, modulus - 2, modulus)

mod_pow: procedure
  parse arg base, exp, modulus
  res = 1
  base = mod(base, modulus)
  do while exp > 0
    if exp // 2 = 1 then
      res = mod(res * base, modulus)
    base = mod(base * base, modulus)
    exp = exp % 2
  end
  return res
