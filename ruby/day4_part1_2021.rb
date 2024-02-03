
input = File.read('input.txt').split("\n\n")
numbers = input.shift.split(',').map(&:to_i)
boards = input.map { |b| b.split("\n").map { |r| r.split.map(&:to_i) } }

def mark_number(board, number)
  board.each { |row| row.map! { |n| n == number ? nil : n } }
end

def winner?(board)
  board.any? { |row| row.compact.empty? } || board.transpose.any? { |col| col.compact.empty? }
end

def score(board, number)
  board.flatten.compact.sum * number
end

numbers.each do |number|
  boards.each do |board|
    mark_number(board, number)
    if winner?(board)
      puts score(board, number)
      exit
    end
  end
end
