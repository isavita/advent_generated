require 'digest'

def find_shortest_path(passcode)
  queue = [[[0, 0], '']]
  while !queue.empty?
    pos, path = queue.shift
    return path if pos == [3, 3]

    hash = Digest::MD5.hexdigest(passcode + path)[0...4]
    ['U', 'D', 'L', 'R'].each_with_index do |dir, i|
      next if hash[i] < 'b' || hash[i] > 'f'
      new_pos = case dir
                when 'U' then [pos[0] - 1, pos[1]]
                when 'D' then [pos[0] + 1, pos[1]]
                when 'L' then [pos[0], pos[1] - 1]
                when 'R' then [pos[0], pos[1] + 1]
                end
      next if new_pos[0] < 0 || new_pos[0] > 3 || new_pos[1] < 0 || new_pos[1] > 3
      queue << [new_pos, path + dir]
    end
  end
  nil
end

passcode = File.read('input.txt').strip
puts find_shortest_path(passcode)
