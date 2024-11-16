
class BingoBoard
  property grid : Array(Array(Int32))
  property marked : Array(Array(Bool))

  def initialize(@grid : Array(Array(Int32)))
    @marked = Array.new(5) { Array.new(5, false) }
  end

  def mark(number : Int32)
    @grid.each_with_index do |row, r|
      row.each_with_index do |val, c|
        @marked[r][c] = true if val == number
      end
    end
  end

  def winner?
    # Check rows
    @marked.each do |row|
      return true if row.all?
    end

    # Check columns
    5.times do |c|
      return true if (0...5).all? { |r| @marked[r][c] }
    end

    false
  end

  def score(last_number : Int32)
    unmarked_sum = 0
    @grid.each_with_index do |row, r|
      row.each_with_index do |val, c|
        unmarked_sum += val unless @marked[r][c]
      end
    end
    unmarked_sum * last_number
  end
end

def solve_bingo(numbers : Array(Int32), boards : Array(BingoBoard))
  numbers.each do |number|
    boards.each do |board|
      board.mark(number)
      if board.winner?
        return board.score(number)
      end
    end
  end
  0
end

# Read input from file
input = File.read("input.txt").split("\n\n")
numbers = input[0].split(",").map(&.to_i)

boards = input[1..-1].map do |board_input|
  grid = board_input.split("\n").map do |row|
    row.split.map(&.to_i)
  end
  BingoBoard.new(grid)
end

puts solve_bingo(numbers, boards)
