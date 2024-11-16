
class Marble
  property value : Int32
  property prev : Marble?
  property next : Marble?

  def initialize(@value : Int32)
  end
end

class MarbleGame
  def self.play(players : Int32, last_marble : Int32) : Int64
    scores = Array.new(players, 0_i64)
    current = Marble.new(0)
    current.next = current
    current.prev = current

    (1..last_marble).each do |marble|
      if marble % 23 == 0
        player = marble % players
        7.times { current = current.prev.not_nil! }
        
        scores[player] += marble + current.value
        
        current.prev.not_nil!.next = current.next
        current.next.not_nil!.prev = current.prev
        current = current.next.not_nil!
      else
        current = current.next.not_nil!
        new_marble = Marble.new(marble)
        new_marble.prev = current
        new_marble.next = current.next
        
        current.next.not_nil!.prev = new_marble
        current.next = new_marble
        current = new_marble
      end
    end

    scores.max
  end

  def self.read_input(filename : String) : Tuple(Int32, Int32)
    File.open(filename) do |file|
      line = file.gets.not_nil!
      parts = line.split
      {
        parts[0].to_i,
        parts[6].to_i
      }
    end
  end
end

players, last_marble = MarbleGame.read_input("input.txt")
puts MarbleGame.play(players, last_marble)
