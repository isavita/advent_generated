
/* Rexx */
main:
  numeric digits 250
  stones. = 0
  keys = ''
  line = linein('input.txt')
  call stream 'input.txt', 'c', 'close'

  do i = 1 to words(line)
    stone = word(line, i)
    if stones.stone = 0 then keys = keys stone
    stones.stone = stones.stone + 1
  end
  keys = strip(keys)

  do 75
    new_stones. = 0
    new_keys = ''

    do i = 1 to words(keys)
      stone = word(keys, i)
      count = stones.stone
      if stone = '0' then do
        call add_stone '1', count
      end
      else if length(stone) // 2 = 0 then do
        parse value split_stone(stone) with left right
        call add_stone left, count
        call add_stone right, count
      end
      else do
        new_stone = stone * 2024
        call add_stone new_stone, count
      end
    end

    stones. = 0
    keys = strip(new_keys)
    do i = 1 to words(keys)
      key = word(keys, i)
      stones.key = new_stones.key
    end
  end

  total_stones = 0
  do i = 1 to words(keys)
    stone = word(keys, i)
    total_stones = total_stones + stones.stone
  end
  say total_stones
exit

add_stone: procedure expose new_stones. new_keys
  arg stone, count
  if new_stones.stone = 0 then new_keys = new_keys stone
  new_stones.stone = new_stones.stone + count
  return

split_stone: procedure
  arg s
  mid = length(s) % 2
  left = substr(s, 1, mid)
  right = substr(s, mid + 1)
  return trim_leading_zeros(left) trim_leading_zeros(right)

trim_leading_zeros: procedure
  arg s
  do i = 1 to length(s) - 1
    if substr(s, i, 1) \= '0' then return substr(s, i)
  end
  return substr(s, length(s))
