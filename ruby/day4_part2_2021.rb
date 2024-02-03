
input = File.read('input.txt').split("\n\n")
numbers = input.shift.split(',').map(&:to_i)
boards = input.map { |b| b.split("\n").map { |r| r.split.map(&:to_i) } }

def mark_number(board, number)
  board.each_with_index do |row, i|
    row.each_with_index do |cell, j|
      board[i][j] = nil if cell == number
    end
  end
end

def board_won?(board)
  board.any? { |row| row.compact.empty? } || board.transpose.any? { |col| col.compact.empty? }
end

def score(board, number)
  board.flatten.compact.sum * number
end

def play_bingo(numbers, boards)
  numbers.each do |number|
    boards.each do |board|
      mark_number(board, number)
      return score(board, number) if board_won?(board)
    end
  end
end

def play_bingo_to_lose(numbers, boards)
  last_score = 0
  numbers.each do |number|
    boards.each_with_index do |board, i|
      next if board.nil?
      
      mark_number(board, number)
      if board_won?(board)
        last_score = score(board, number)
        boards[i] = nil
      end
    end
    return last_score if boards.compact.empty?
  end
end

puts play_bingo(numbers, boards.map(&:dup))
puts play_bingo_to_lose(numbers, boards)
