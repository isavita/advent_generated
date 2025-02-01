
def solve
  raw = File.readlines("input.txt").map(&:strip).reject(&:empty?)
  return 0 if raw.length % 7 != 0

  locks = []
  keys = []

  raw.each_slice(7) do |block|
    next unless block.all? { |line| line.length >= 5 }
    if block[0].chars.all? { |c| c == '#' }
      locks << parse_lock(block)
    else
      keys << parse_key(block)
    end
  end

  count = 0
  locks.each do |lock|
    keys.each do |key|
      count += 1 if fits(lock, key)
    end
  end
  puts count
end


def parse_lock(block)
  (0..4).map do |c|
    cnt = 0
    (1..6).each do |r|
      if block[r][c] == '#'
        cnt += 1
      else
        break
      end
    end
    cnt
  end
end

def parse_key(block)
   (0..4).map do |c|
    cnt = 0
    (0..5).reverse_each do |r|
      if block[r][c] == '#'
        cnt += 1
      else
        break
      end
    end
    cnt
  end
end


def fits(lock, key)
  (0..4).all? { |i| lock[i] + key[i] <= 5 }
end

solve
