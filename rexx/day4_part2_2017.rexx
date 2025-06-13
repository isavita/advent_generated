
/* REXX */
main:
  part1_valid = 0
  part2_valid = 0
  input_file = "input.txt"

  do while lines(input_file) > 0
    line = linein(input_file)
    word_count = words(line)

    seen1. = 0
    is_valid1 = 1
    do i = 1 to word_count
      word = word(line, i)
      if seen1.word = 1 then do
        is_valid1 = 0
        leave
      end
      seen1.word = 1
    end
    if is_valid1 then part1_valid = part1_valid + 1

    seen2. = 0
    is_valid2 = 1
    do i = 1 to word_count
      word = word(line, i)
      sorted_word = sort_chars(word)
      if seen2.sorted_word = 1 then do
        is_valid2 = 0
        leave
      end
      seen2.sorted_word = 1
    end
    if is_valid2 then part2_valid = part2_valid + 1
  end
  call lineout input_file

  say part1_valid
  say part2_valid
return

sort_chars: procedure
  arg str
  len = length(str)
  if len < 2 then return str
  do i = 1 to len
    chars.i = substr(str, i, 1)
  end
  do i = 1 to len - 1
    do j = i + 1 to len
      if chars.i > chars.j then do
        temp = chars.i
        chars.i = chars.j
        chars.j = temp
      end
    end
  end
  sorted_str = ''
  do i = 1 to len
    sorted_str = sorted_str || chars.i
  end
return sorted_str
