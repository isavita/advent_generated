
fs = require 'fs'

class Room
  constructor: (name) ->
    @name = name
    @connections = {}

class Emulator
  constructor: (program, input_values = []) ->
    @memory = program.slice()
    @input = input_values.slice()
    @ip = 0
    @relative_base = 0

  write_string: (s) ->
    for char in s
      @input.push char.charCodeAt(0)
    s.length

  emulate: ->
    while true
      if @ip >= @memory.length
        @memory = @memory.concat(Array(@ip - @memory.length + 1).fill(0))
      instruction = @memory[@ip]
      opcode = instruction % 100

      get_parameter = (offset) =>
        mode = Math.floor(instruction / (10 ** (offset + 1))) % 10
        param = @memory[@ip + offset]
        switch mode
          when 0
            if param >= @memory.length
              @memory = @memory.concat(Array(param - @memory.length + 1).fill(0))
            @memory[param]
          when 1
            param
          when 2
            address = @relative_base + param
            if address >= @memory.length
              @memory = @memory.concat(Array(address - @memory.length + 1).fill(0))
            @memory[address]

      get_write_address = (offset) =>
        mode = Math.floor(instruction / (10 ** (offset + 1))) % 10
        param = @memory[@ip + offset]
        switch mode
          when 0
            address = param
          when 2
            address = @relative_base + param
        if address >= @memory.length
          @memory = @memory.concat(Array(address - @memory.length + 1).fill(0))
        address

      switch opcode
        when 1
          [a, b, c] = [get_parameter(1), get_parameter(2), get_write_address(3)]
          @memory[c] = a + b
          @ip += 4
        when 2
          [a, b, c] = [get_parameter(1), get_parameter(2), get_write_address(3)]
          @memory[c] = a * b
          @ip += 4
        when 3
          if !@input.length
            return [null, 2]
          a = get_write_address(1)
          @memory[a] = @input.shift()
          @ip += 2
        when 4
          a = get_parameter(1)
          @ip += 2
          return [a, 1]
        when 5
          [a, b] = [get_parameter(1), get_parameter(2)]
          @ip = if a != 0 then b else @ip + 3
        when 6
          [a, b] = [get_parameter(1), get_parameter(2)]
          @ip = if a == 0 then b else @ip + 3
        when 7
          [a, b, c] = [get_parameter(1), get_parameter(2), get_write_address(3)]
          @memory[c] = if a < b then 1 else 0
          @ip += 4
        when 8
          [a, b, c] = [get_parameter(1), get_parameter(2), get_write_address(3)]
          @memory[c] = if a == b then 1 else 0
          @ip += 4
        when 9
          a = get_parameter(1)
          @relative_base += a
          @ip += 2
        when 99
          return [null, 0]

find_path = (from_room, to_room) ->
    queue = [[from_room, [from_room]]]
    visited = new Set([from_room.name])

    while queue.length
        [current, path] = queue.shift()
        if current == to_room
            return path
        for neighbor in Object.values(current.connections)
            if neighbor and neighbor.name not in visited
                visited.add(neighbor.name)
                queue.push([neighbor, path.concat([neighbor])])
    null

main = ->
  text = fs.readFileSync("input.txt", 'utf8').trim()
  program = (parseInt(value) for value in text.split(","))
  emulator = new Emulator(program)

  send_command = (format_str, ...args) ->
    cmd = format_str.replace /%s/g, () -> args.shift()
    emulator.write_string cmd

  opposite = {north: "south", south: "north", west: "east", east: "west"}
  world = {}
  inventory = {}
  mode = 0
  path = []
  checkpoint = null
  floor = null
  test_dir = ""
  available_items = []
  item_mask = 0
  last = null
  last_items = []
  last_dir = ""
  output_builder = []
  current_room = null
  
  while true
    [char, status] = emulator.emulate()

    if status == 0
      output = String.fromCharCode(...output_builder)
      for line in output.split("\n")
        match = line.match(/"Oh, hello! You should be able to get in by typing (\d+) on the keypad at the main airlock\."$/)
        if match
          console.log match[1]
          return

    else if status == 1
      if char?
        output_builder.push char

    else if status == 2
      output = String.fromCharCode(...output_builder)
      output_builder = []

      items = []
      lines = output.split("\n")
      i = 0
      while i < lines.length
        line = lines[i].trim()

        if !line or line == "Command?"
          i++
          continue

        match = line.match(/^== (.+) ==$/)
        if match
          name = match[1]
          i++
          i++ while i < lines.length and lines[i].trim() != ""
          if not world[name]
            current_room = new Room(name)
            world[name] = current_room
          else
            current_room = world[name]
          items = []
          continue

        if line == "Doors here lead:"
          i++
          while i < lines.length and lines[i].trim() != ""
            door_line = lines[i].trim()
            match = door_line.match(/^- (.+)$/)
            if match and current_room
              direction = match[1]
              if not current_room.connections[direction]?
                current_room.connections[direction] = null
            i++
          continue

        if line == "Items here:"
          i++
          while i < lines.length and lines[i].trim() != ""
            item_line = lines[i].trim()
            match = item_line.match(/^- (.+)$/)
            if match
              item = match[1]
              items.push item
            i++
          continue

        match = line.match(/^You take the (.+)\.$/)
        if match
          taken = match[1]
          inventory[taken] = true
          if last
            current_room = last
            items = (item for item in last_items when item != taken)
          i++
          continue
            
        match = line.match(/^You drop the (.+)\.$/)
        if match
            dropped = match[1]
            inventory[dropped] = false
            if last
              current_room = last
              items = last_items.concat([dropped])
            i++
            continue
          
        if line.startsWith('A loud, robotic voice says "Alert!')
          if mode == 0
            path.pop() if path.length
            [checkpoint, floor, test_dir] = [last, current_room, last_dir]
            if checkpoint and test_dir
              checkpoint.connections[test_dir] = floor
          [last, last_items, last_dir] = [null, [], ""]
          i++
          continue

        i++
      
      if last? and last_dir and current_room?
        if not last.connections[last_dir]?
          last.connections[last_dir] = current_room
          current_room.connections[opposite[last_dir]] = last

      [last, last_items, last_dir] = [current_room, items, ""]
      
      if mode == 0
        blacklist = ["photons", "escape pod", "molten lava", "infinite loop", "giant electromagnet"]
        take_item = null
        for item in items
          if item not in blacklist
              take_item = item
              break
        if take_item
          send_command "take %s\n", take_item
          continue

        target = null
        for direction, room of current_room.connections
          if not room?
            path.push current_room
            target = direction
            break

        if target
          last_dir = target
          send_command "%s\n", target
          continue

        if path.length
          last_room = path.pop()
          back_dir = null
          for direction, room of current_room.connections
            if room == last_room
              back_dir = direction
              break
          if back_dir
            last_dir = back_dir
            send_command "%s\n", back_dir
            continue
          else
            throw new Error "Cannot go from \"#{current_room.name}\" to \"#{last_room.name}\""
        
        if checkpoint and floor
           new_path = find_path current_room, checkpoint
           if new_path
             path = new_path.slice(1)
           mode = 1
           continue

      else if mode == 1
          if path.length
            next_room = path.shift()
            direction = null
            for dir, room of current_room.connections
              if room == next_room
                direction = dir
                break
            if direction
                last_dir = direction
                send_command "%s\n", direction
                continue
            else
                throw new Error "Cannot go from \"#{current_room.name}\" to \"#{next_room.name}\""
          else
            available_items = (item for item, has of inventory when has)
            item_mask = 0
            mode = 2
            
      else if mode == 2
          take_drop_item = null
          for index,item of available_items
              target_state = (item_mask & (1 << index)) != 0
              if inventory[item] != target_state
                action = if target_state then "take" else "drop"
                take_drop_item = [action,item]
                break
          if take_drop_item
              [action, item] = take_drop_item
              send_command "%s %s\n", action, item
              continue

          item_mask++
          if test_dir
            send_command "%s\n", test_dir
            continue
          else
            throw new Error "Test direction (test_dir) is not set."
            
          continue

main()
