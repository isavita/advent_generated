file = File.open("input.txt", "r")
input = file.gets_to_end.strip
file.close

nums, boards = parse_input(input)

last_winning_score = -1
already_won = Hash(Int32, Bool).new
nums.each do |n|
  boards.each_with_index do |b, i|
    next if already_won[i]?
    if b.pick_num(n)
      last_winning_score = b.score * n
      already_won[i] = true
    end
  end
end

puts last_winning_score

class BoardState
  getter board : Array(Array(Int32))
  getter picked : Array(Array(Bool))

  def initialize(@board)
    @picked = Array.new(@board.size) { Array.new(@board[0].size, false) }
  end

  def pick_num(num : Int32) : Bool
    @board.each_with_index do |row, r|
      row.each_with_index do |v, c|
        if v == num
          @picked[r][c] = true
        end
      end
    end

    (0...@board.size).each do |i|
      is_full_row = true
      is_full_col = true

      (0...@board[0].size).each do |j|
        if !@picked[i][j]
          is_full_row = false
        end

        if !@picked[j][i]
          is_full_col = false
        end
      end

      return true if is_full_row || is_full_col
    end

    false
  end

  def score : Int32
    score = 0
    @board.each_with_index do |row, r|
      row.each_with_index do |v, c|
        score += v unless @picked[r][c]
      end
    end
    score
  end
end

def parse_input(input : String) : Tuple(Array(Int32), Array(BoardState))
  lines = input.split("\n\n")

  nums = lines[0].split(',').map { |v| v.to_i }
  boards = lines[1..].map do |grid|
    board = grid.split("\n").map do |line|
      line.gsub(/\s{2,}/, " ").gsub(/^\s+/, "").split(" ").map { |v| v.to_i }
    end
    BoardState.new(board)
  end

  {nums, boards}
end