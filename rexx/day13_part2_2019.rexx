
/* REXX */
main:
  parse arg in_file
  if in_file = '' then in_file = 'input.txt'

  /* Initialize computer state */
  mem. = 0
  ip = 0
  relative_base = 0
  halted = 0
  score = 0
  ball_x = 0
  paddle_x = 0
  output_state = 0
  temp_x = 0
  temp_y = 0

  /* Read and parse program from input file */
  program_string = linein(in_file)
  i = 0
  do while program_string <> ''
    parse var program_string mem.i ',' program_string
    i = i + 1
  end

  mem.0 = 2 /* Insert 2 quarters for free play */

  /* Main execution loop */
  do while \halted
    instruction = mem.ip
    opcode = instruction // 100
    modes = right(instruction % 100, 3, '0')

    select
      when opcode = 1 then do /* Add */
        p1 = get_param(1, substr(modes, 3, 1))
        p2 = get_param(2, substr(modes, 2, 1))
        addr = get_write_addr(3, substr(modes, 1, 1))
        mem.addr = p1 + p2
        ip = ip + 4
      end
      when opcode = 2 then do /* Multiply */
        p1 = get_param(1, substr(modes, 3, 1))
        p2 = get_param(2, substr(modes, 2, 1))
        addr = get_write_addr(3, substr(modes, 1, 1))
        mem.addr = p1 * p2
        ip = ip + 4
      end
      when opcode = 3 then do /* Input */
        addr = get_write_addr(1, substr(modes, 3, 1))
        input_val = 0
        if ball_x > paddle_x then input_val = 1
        if ball_x < paddle_x then input_val = -1
        mem.addr = input_val
        ip = ip + 2
      end
      when opcode = 4 then do /* Output */
        output_val = get_param(1, substr(modes, 3, 1))
        if output_state = 0 then do
          temp_x = output_val
          output_state = 1
        end
        else if output_state = 1 then do
          temp_y = output_val
          output_state = 2
        end
        else do
          tile_id = output_val
          if temp_x = -1 & temp_y = 0 then
            score = tile_id
          else do
            if tile_id = 3 then paddle_x = temp_x
            if tile_id = 4 then ball_x = temp_x
          end
          output_state = 0
        end
        ip = ip + 2
      end
      when opcode = 5 then do /* Jump-if-true */
        p1 = get_param(1, substr(modes, 3, 1))
        p2 = get_param(2, substr(modes, 2, 1))
        if p1 <> 0 then ip = p2
        else ip = ip + 3
      end
      when opcode = 6 then do /* Jump-if-false */
        p1 = get_param(1, substr(modes, 3, 1))
        p2 = get_param(2, substr(modes, 2, 1))
        if p1 = 0 then ip = p2
        else ip = ip + 3
      end
      when opcode = 7 then do /* Less than */
        p1 = get_param(1, substr(modes, 3, 1))
        p2 = get_param(2, substr(modes, 2, 1))
        addr = get_write_addr(3, substr(modes, 1, 1))
        if p1 < p2 then mem.addr = 1
        else mem.addr = 0
        ip = ip + 4
      end
      when opcode = 8 then do /* Equals */
        p1 = get_param(1, substr(modes, 3, 1))
        p2 = get_param(2, substr(modes, 2, 1))
        addr = get_write_addr(3, substr(modes, 1, 1))
        if p1 = p2 then mem.addr = 1
        else mem.addr = 0
        ip = ip + 4
      end
      when opcode = 9 then do /* Adjust relative base */
        p1 = get_param(1, substr(modes, 3, 1))
        relative_base = relative_base + p1
        ip = ip + 2
      end
      when opcode = 99 then /* Halt */
        halted = 1
      otherwise /* Unknown opcode */
        halted = 1
    end
  end

  say score
return

get_param: procedure expose ip relative_base mem.
  parse arg offset, mode
  addr_param = ip + offset
  select
    when mode = 0 then addr = mem.addr_param /* Position Mode */
    when mode = 1 then return mem.addr_param /* Immediate Mode */
    when mode = 2 then addr = relative_base + mem.addr_param /* Relative Mode */
  end
return mem.addr

get_write_addr: procedure expose ip relative_base mem.
  parse arg offset, mode
  addr_param = ip + offset
  select
    when mode = 0 then addr = mem.addr_param /* Position Mode */
    when mode = 2 then addr = relative_base + mem.addr_param /* Relative Mode */
  end
return addr
