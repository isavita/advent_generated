
class Day11
  record Half, is_chip : Bool, material : String
  record State, floors : Array(Array(Half)), elevator : Int32, steps : Int32

  def self.read_input
    floors = Array.new(4) { Array(Half).new }
    File.read("input.txt").each_line.with_index do |line, idx|
      words = line.gsub(/[,.]/, " ").split
      words.each_index do |i|
        next_word = words[i]?
        case next_word
        when "generator"
          floors[idx] << Half.new(is_chip: false, material: words[i - 1])
        when "microchip"
          material = words[i - 1].rstrip('-').split('-')[0]
          floors[idx] << Half.new(is_chip: true, material: material)
        end
      end
    end
    State.new(floors: floors, elevator: 0, steps: 0)
  end

  def self.hash_key(s : State) : String
    gens = {} of String => Int32
    chips = {} of String => Int32
    s.floors.each_with_index do |fl, fl_idx|
      fl.each do |half|
        if half.is_chip
          chips[half.material] = fl_idx
        else
          gens[half.material] = fl_idx
        end
      end
    end
    pairs = gens.keys.map { |m| {gens[m], chips[m]} }.sort_by { |a| {a[0], a[1]} }
    "#{s.elevator}#{pairs}"
  end

  def self.valid?(s : State) : Bool
    s.floors.each do |fl|
      gens = fl.select { |h| !h.is_chip }.map(&.material)
      next if gens.empty?
      fl.each do |half|
        return false if half.is_chip && !gens.includes?(half.material)
      end
    end
    true
  end

  def self.done?(s : State) : Bool
    s.floors[0..2].sum(&.size) == 0
  end

  def self.moves(s : State) : Array(Array(Int32))
    cur = s.floors[s.elevator]
    perms = [] of Array(Int32)
    cur.size.times do |i|
      (i + 1...cur.size).each { |j| perms << [i, j] }
      perms << [i]
    end
    perms
  end

  def self.next_states(s : State) : Array(State)
    res = [] of State
    moves(s).each do |perm|
      dirs = [] of Int32
      dirs << 1 if s.elevator < 3
      dirs << -1 if s.elevator > 0
      dirs.each do |d|
        ns = State.new(
          floors: s.floors.map(&.dup),
          elevator: s.elevator + d,
          steps: s.steps + 1
        )
        old = s.elevator
        perm.reverse_each do |idx|
          ns.floors[ns.elevator] << s.floors[old][idx]
          ns.floors[old].delete_at(idx)
        end
        res << ns if valid?(ns)
      end
    end
    res
  end

  def self.solve(part)
    state = read_input
    if part == 2
      state.floors[0] += [
        Half.new(is_chip: false, material: "elerium"),
        Half.new(is_chip: true, material: "elerium"),
        Half.new(is_chip: false, material: "dilithium"),
        Half.new(is_chip: true, material: "dilithium")
      ]
    end
    seen = Set(String).new
    queue = Deque(State).new
    queue << state
    until queue.empty?
      cur = queue.shift
      return cur.steps if done?(cur)
      key = hash_key(cur)
      next if seen.includes?(key)
      seen << key
      next_states(cur).each { |st| queue << st }
    end
    -1
  end
end

puts Day11.solve(1)
