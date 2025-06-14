
/* REXX */
call main

main:
  line = linein('input.txt')
  call stream 'input.txt', 'c', 'close'

  banks.0 = 0
  do i = 1 while word(line, i) <> ''
    banks.i = word(line, i)
    banks.0 = i
  end
  num_banks = banks.0

  cycles = 0
  drop seen.

  do forever
    state_key = ''
    do i = 1 to num_banks
      state_key = state_key banks.i'.'
    end

    if seen.state_key == 1 then leave
    seen.state_key = 1

    max_blocks = -1
    max_index = 0
    do i = 1 to num_banks
      if banks.i > max_blocks then do
        max_blocks = banks.i
        max_index = i
      end
    end

    blocks_to_move = banks.max_index
    banks.max_index = 0
    current_pos = max_index
    do i = 1 to blocks_to_move
      current_pos = current_pos + 1
      if current_pos > num_banks then
        current_pos = 1
      banks.current_pos = banks.current_pos + 1
    end

    cycles = cycles + 1
  end

  say cycles
return
