
class Marble
  attr_accessor :value, :prev, :next

  def initialize(value)
    @value = value
    @prev = nil
    @next = nil
  end
end

def read_input(filename)
  File.open(filename, 'r') do |file|
    line = file.readline
    parts = line.split(' ')
    players = parts[0].to_i
    last_marble = parts[6].to_i
    return players, last_marble
  end
end

def play_marble_game(players, last_marble)
  scores = Array.new(players, 0)
  current = Marble.new(0)
  current.next = current
  current.prev = current

  (1..last_marble).each do |marble|
    if marble % 23 == 0
      player = marble % players
      7.times { current = current.prev }
      scores[player] += marble + current.value
      current.prev.next = current.next
      current.next.prev = current.prev
      current = current.next
    else
      current = current.next
      new_marble = Marble.new(marble)
      new_marble.prev = current
      new_marble.next = current.next
      current.next.prev = new_marble
      current.next = new_marble
      current = new_marble
    end
  end

  scores.max
end

players, last_marble = read_input('input.txt')
last_marble *= 100
puts play_marble_game(players, last_marble)
