using Printf

mutable struct BingoBoard
    numbers::Matrix{Int}
    marked::Matrix{Bool}
end

function mark!(board::BingoBoard, number::Int)
    for i in 1:5, j in 1:5
        if board.numbers[i, j] == number
            board.marked[i, j] = true
        end
    end
end

function has_won(board::BingoBoard)
    for i in 1:5
        if is_row_marked(board.marked, i) || is_column_marked(board.marked, i)
            return true
        end
    end
    return false
end

function unmarked_sum(board::BingoBoard)
    sum = 0
    for i in 1:5, j in 1:5
        if !board.marked[i, j]
            sum += board.numbers[i, j]
        end
    end
    return sum
end

function is_row_marked(marked::Matrix{Bool}, row::Int)
    for j in 1:5
        if !marked[row, j]
            return false
        end
    end
    return true
end

function is_column_marked(marked::Matrix{Bool}, column::Int)
    for i in 1:5
        if !marked[i, column]
            return false
        end
    end
    return true
end

function main()
    open("input.txt", "r") do file
        lines = readlines(file)
        numbers = parse.(Int, split(lines[1], ","))
        boards = []
        for i in 3:6:length(lines)
            board_numbers = Vector{Int}()
            for line in lines[i:i+4]
                push!(board_numbers, parse.(Int, split(line, r"[\s]+", keepempty=false))...)
            end
            board = BingoBoard(
                reshape(board_numbers, 5, 5),
                falses(5, 5)
            )
            push!(boards, board)
        end

        winning_board = nothing
        winning_number = 0
        for number in numbers
            for board in boards
                mark!(board, number)
                if has_won(board)
                    winning_board = board
                    winning_number = number
                    break
                end
            end
            if winning_board !== nothing
                break
            end
        end

        @printf("%d\n", unmarked_sum(winning_board) * winning_number)
    end
end

main()