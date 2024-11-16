
def get_value(arg : String, registers : Hash(String, Int64)) : Int64
  arg.to_i64?  || registers[arg]
end

def main
  instructions = File.read_lines("input.txt").map(&.split)

  registers0 = {"p" => 0_i64}
  registers1 = {"p" => 1_i64}
  queue0 = [] of Int64
  queue1 = [] of Int64
  send_count1 = 0
  i0 = 0
  i1 = 0
  deadlock0 = false
  deadlock1 = false

  while !(deadlock0 && deadlock1)
    deadlock0 = true
    deadlock1 = true

    # Program 0
    while i0 < instructions.size
      instruction = instructions[i0]
      cmd = instruction[0]
      arg1 = instruction[1]

      case cmd
      when "snd"
        queue1 << get_value(arg1, registers0)
      when "set"
        registers0[arg1] = get_value(instruction[2], registers0)
      when "add"
        registers0[arg1] += get_value(instruction[2], registers0)
      when "mul"
        registers0[arg1] *= get_value(instruction[2], registers0)
      when "mod"
        registers0[arg1] %= get_value(instruction[2], registers0)
      when "rcv"
        if queue0.empty?
          break
        end
        registers0[arg1] = queue0.shift
      when "jgz"
        if get_value(arg1, registers0) > 0
          i0 += get_value(instruction[2], registers0)
          next
        end
      end

      i0 += 1
      deadlock0 = false
    end

    # Program 1
    while i1 < instructions.size
      instruction = instructions[i1]
      cmd = instruction[0]
      arg1 = instruction[1]

      case cmd
      when "snd"
        queue0 << get_value(arg1, registers1)
        send_count1 += 1
      when "set"
        registers1[arg1] = get_value(instruction[2], registers1)
      when "add"
        registers1[arg1] += get_value(instruction[2], registers1)
      when "mul"
        registers1[arg1] *= get_value(instruction[2], registers1)
      when "mod"
        registers1[arg1] %= get_value(instruction[2], registers1)
      when "rcv"
        if queue1.empty?
          break
        end
        registers1[arg1] = queue1.shift
      when "jgz"
        if get_value(arg1, registers1) > 0
          i1 += get_value(instruction[2], registers1)
          next
        end
      end

      i1 += 1
      deadlock1 = false
    end
  end

  puts send_count1
end

main
