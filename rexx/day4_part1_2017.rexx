
/* REXX */
call main
exit

main:
  valid_count = 0
  file = 'input.txt'
  do while lines(file) > 0
    line = linein(file)
    is_valid = 1
    drop seen.
    do i = 1 to words(line)
      word = word(line, i)
      if seen.word = 1 then do
        is_valid = 0
        leave
      end
      seen.word = 1
    end
    if is_valid then
      valid_count = valid_count + 1
  end
  say valid_count
return
