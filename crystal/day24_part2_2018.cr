
struct Parsed
  property units : Int32
  property hp : Int32
  property damage : Int32
  property atype : String
  property init : Int32
  property imm : Array(String)
  property weak : Array(String)
  property army : Int32

  def initialize(@units, @hp, @damage, @atype, @init, @imm, @weak, @army)
  end
end

class Group
  property units : Int32
  property hp : Int32
  property damage : Int32
  property atype : String
  property init : Int32
  property imm : Array(String)
  property weak : Array(String)
  property target : Group?
  property attacker : Group?

  def initialize(@units, @hp, @damage, @atype, @init, @imm, @weak)
    @target = nil
    @attacker = nil
  end

  def effective_power
    units.to_i64 * damage
  end

  def damage_to(e : Group)
    return 0 if e.imm.includes?(atype)
    return effective_power * 2 if e.weak.includes?(atype)
    effective_power
  end
end

def parse(lines : Array(String)) : Array(Parsed)
  header = /(.*):/
  group_r = /(\d+) units each with (\d+) hit points(.*) with an attack that does (\d+) (\w+) damage at initiative (\d+)/
  imm_r = /immune to ([^;)]+)/
  weak_r = /weak to ([^;)]+)/
  army = 0
  lines.compact_map do |line|
    if line =~ header
      army = $1 == "Immune System" ? 1 : 2
      nil
    elsif line =~ group_r
      u, h, extra, d, t, i = $1.to_i, $2.to_i, $3, $4.to_i, $5, $6.to_i
      imm = imm_r.match(extra).try(&.[1].split(", ")) || [] of String
      weak = weak_r.match(extra).try(&.[1].split(", ")) || [] of String
      Parsed.new(u, h, d, t, i, imm, weak, army)
    else
      nil
    end
  end
end

def reset(parsed : Array(Parsed), boost : Int32)
  armies = [Array(Group).new, Array(Group).new, Array(Group).new]
  init = Array(Group).new
  parsed.each do |p|
    g = Group.new(p.units, p.hp, p.damage + (p.army == 1 ? boost : 0), p.atype, p.init, p.imm, p.weak)
    armies[p.army] << g
    init << g
  end
  {armies, init}
end

def total_units(armies : Array(Array(Group)))
  armies[1].sum(&.units) + armies[2].sum(&.units)
end

def find_targets(armies : Array(Array(Group)))
  (1..2).each do |a|
    opp = armies[3 - a]
    armies[a].sort_by! { |g| {-g.effective_power, -g.init} }
    armies[a].each do |g|
      next unless g.units > 0
      cand = opp.select { |e| e.units > 0 && e.attacker.nil? }.map { |e| {g.damage_to(e), e.effective_power, e.init, e} }.select(&.[0].> 0)
      if cand.size > 0
        best = cand.max_by { |t| t[0..2] }[3]
        g.target = best
        best.attacker = g
      end
    end
  end
end

def attack(init : Array(Group))
  init.sort_by! { |g| -g.init }
  init.each do |g|
    next unless g.units > 0 && g.target && g.target.not_nil!.units > 0
    dmg = g.damage_to(g.target.not_nil!)
    killed = (dmg // g.target.not_nil!.hp).to_i
    g.target.not_nil!.units = Math.max(0, g.target.not_nil!.units - killed)
  end
  init.each do |g|
    g.target = nil
    g.attacker = nil
  end
end

def clean(armies : Array(Array(Group)), init : Array(Group))
  (1..2).each { |a| armies[a].reject! { |g| g.units <= 0 } }
  init.reject! { |g| g.units <= 0 }
end

lines = File.read_lines("input.txt")
parsed = parse(lines)
boost = 0
loop do
  armies, init = reset(parsed, boost)
  stalemate = false
  loop do
    break if armies[1].empty? || armies[2].empty?
    before = total_units(armies)
    find_targets(armies)
    attack(init)
    after = total_units(armies)
    if after == before
      stalemate = true
      break
    end
    clean(armies, init)
  end
  if !stalemate && armies[1].size > 0 && armies[2].empty?
    puts total_units(armies)
    break
  end
  boost += 1
end
