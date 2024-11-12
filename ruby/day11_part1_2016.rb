
require 'set'

class Halves
  attr_reader :is_chip, :material

  def initialize(is_chip, material)
    @is_chip = is_chip
    @material = material
  end

  def to_s
    type = @is_chip ? " microchip" : " generator"
    "#{@material}#{type}"
  end
end

class State
  attr_reader :floors, :elevator_level, :steps

  def initialize(floors, elevator_level = 0, steps = 0)
    @floors = floors
    @elevator_level = elevator_level
    @steps = steps
  end

  def self.from_input(input)
    floors = Array.new(4) { [] }
    input.split("\n").each_with_index do |line, line_index|
      parts = line.gsub(/[,.]/, '').split
      parts.each_with_index do |word, i|
        if word == 'generator'
          floors[line_index] << Halves.new(false, parts[i-1])
        elsif word == 'microchip'
          material = parts[i-1].split('-').first
          floors[line_index] << Halves.new(true, material)
        end
      end
    end
    new(floors)
  end

  def hash_key
    gen_map = {}
    chip_map = {}
    @floors.each_with_index do |floor, fl_index|
      floor.each do |half|
        if half.is_chip
          chip_map[half.material] = fl_index
        else
          gen_map[half.material] = fl_index
        end
      end
    end

    gen_chip_pairs = gen_map.keys.map { |material| [gen_map[material], chip_map[material]] }
    [@elevator_level, gen_chip_pairs.sort].hash
  end

  def valid?
    @floors.each do |floor|
      gens = floor.reject { |half| half.is_chip }.map(&:material).to_set
      next if gens.empty?

      chips = floor.select { |half| half.is_chip }
      return false if chips.any? { |chip| !gens.include?(chip.material) }
    end
    true
  end

  def done?
    @floors[0..2].sum(&:length) == 0
  end

  def movable_perm_indices
    current_level = @floors[@elevator_level]
    (0...current_level.length).flat_map do |i|
      [(i), (i+1...current_level.length).map { |j| [i, j] }].flatten(1)
    end
  end

  def clone
    State.new(@floors.map(&:dup), @elevator_level, @steps)
  end

  def next_states
    states = []
    elevator_diffs = []
    elevator_diffs << 1 if @elevator_level < @floors.length - 1
    elevator_diffs << -1 if @elevator_level > 0

    elevator_diffs.each do |ele_diff|
      movable_perm_indices.each do |perm_indices|
        perm_indices = [perm_indices].flatten
        cl = clone
        cl.instance_variable_set(:@elevator_level, @elevator_level + ele_diff)
        cl.instance_variable_set(:@steps, @steps + 1)
        old_level = @elevator_level
        new_level = cl.elevator_level

        perm_indices.each do |index|
          cl.floors[new_level] << cl.floors[old_level][index]
        end

        perm_indices.reverse_each do |index|
          cl.floors[old_level].delete_at(index)
        end

        states << cl if cl.valid?
      end
    end
    states
  end
end

def rtg_hell_day(input)
  current_state = State.from_input(input)
  queue = [current_state]
  prev_states = Set.new

  until queue.empty?
    front = queue.shift

    return front.steps if front.done?

    hash = front.hash_key
    next if prev_states.include?(hash)
    prev_states << hash

    queue.concat(front.next_states)
  end

  -1
end

input = File.read('input.txt').chomp
puts rtg_hell_day(input)
