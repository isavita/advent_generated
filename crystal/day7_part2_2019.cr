
module IntCode
  def self.permutations(arr)
    res = [] of Array(Int32)
    helper = uninitialized Proc(Array(Int32), Int32, Nil)
    helper = ->(arr : Array(Int32), n : Int32) {
      if n == 1
        res << arr.dup
      else
        n.times do |i|
          helper.call(arr, n - 1)
          idx = n.odd? ? i : 0
          arr[idx], arr[n - 1] = arr[n - 1], arr[idx]
        end
      end
    }
    helper.call(arr, arr.size)
    res
  end

  def self.run_loop(phase : Array(Int32), code : Array(Int32)) : Int32
    outputs = Array.new(5) { Deque(Int32).new }
    inputs = [
      Deque{phase[0], 0},
      Deque{phase[1]},
      Deque{phase[2]},
      Deque{phase[3]},
      Deque{phase[4]},
    ]

    codes = Array.new(5) { code.dup }
    ips = Array.new(5, 0)
    input_indexes = Array.new(5, 0)
    halted = Array.new(5, false)
    amp = 0

    get_param = ->(code : Array(Int32), ip : Int32, param : Int32, immediate : Bool) do
      immediate ? code[param] : code[code[param]]
    end

    while halted.any? { |h| !h }
      ip = ips[amp]
      input_index = input_indexes[amp]
      current_code = codes[amp]

      while true
        cmd = current_code[ip]
        opcode = cmd % 100

        if opcode == 99
          halted[amp] = true
          break
        end

        if opcode == 3 && input_index >= inputs[amp].size
          ips[amp] = ip
          input_indexes[amp] = input_index
          break
        end

        case opcode
        when 1
          param1 = get_param.call(current_code, ip, ip + 1, (cmd // 100) % 10 == 1)
          param2 = get_param.call(current_code, ip, ip + 2, (cmd // 1000) % 10 == 1)
          current_code[current_code[ip + 3]] = param1 + param2
          ip += 4
        when 2
          param1 = get_param.call(current_code, ip, ip + 1, (cmd // 100) % 10 == 1)
          param2 = get_param.call(current_code, ip, ip + 2, (cmd // 1000) % 10 == 1)
          current_code[current_code[ip + 3]] = param1 * param2
          ip += 4
        when 3
          current_code[current_code[ip + 1]] = inputs[amp][input_index]
          input_index += 1
          ip += 2
        when 4
          param1 = get_param.call(current_code, ip, ip + 1, (cmd // 100) % 10 == 1)
          outputs[amp] << param1
          inputs[(amp + 1) % 5] << param1
          ip += 2
        when 5
          param1 = get_param.call(current_code, ip, ip + 1, (cmd // 100) % 10 == 1)
          param2 = get_param.call(current_code, ip, ip + 2, (cmd // 1000) % 10 == 1)
          ip = param1 != 0 ? param2 : ip + 3
        when 6
          param1 = get_param.call(current_code, ip, ip + 1, (cmd // 100) % 10 == 1)
          param2 = get_param.call(current_code, ip, ip + 2, (cmd // 1000) % 10 == 1)
          ip = param1 == 0 ? param2 : ip + 3
        when 7
          param1 = get_param.call(current_code, ip, ip + 1, (cmd // 100) % 10 == 1)
          param2 = get_param.call(current_code, ip, ip + 2, (cmd // 1000) % 10 == 1)
          current_code[current_code[ip + 3]] = param1 < param2 ? 1 : 0
          ip += 4
        when 8
          param1 = get_param.call(current_code, ip, ip + 1, (cmd // 100) % 10 == 1)
          param2 = get_param.call(current_code, ip, ip + 2, (cmd // 1000) % 10 == 1)
          current_code[current_code[ip + 3]] = param1 == param2 ? 1 : 0
          ip += 4
        end
      end

      amp = (amp + 1) % 5
    end

    outputs[4].last
  end
end

code = File.read("input.txt").split(',').map(&.to_i)
max = 0
IntCode.permutations([5, 6, 7, 8, 9]).each do |phase|
  res = IntCode.run_loop(phase, code)
  max = res if res > max
end
puts max
