struct BoardState
    board::Vector{Vector{Int}}
    picked::Vector{Vector{Bool}}
end

function BoardState(board::Vector{Vector{Int}})
    picked = [fill(false, length(row)) for row in board]
    return BoardState(board, picked)
end

function pick_num!(b::BoardState, num::Int)
    for (r, rows) in enumerate(b.board)
        for (c, v) in enumerate(rows)
            if v == num
                b.picked[r][c] = true
            end
        end
    end

    for i in 1:length(b.board)
        is_full_row = is_full_col = true

        for j in 1:length(b.board)
            if !b.picked[i][j]
                is_full_row = false
            end

            if !b.picked[j][i]
                is_full_col = false
            end
        end

        if is_full_row || is_full_col
            return true
        end
    end

    return false
end

function score(b::BoardState)
    score = 0

    for (r, rows) in enumerate(b.board)
        for (c, v) in enumerate(rows)
            if !b.picked[r][c]
                score += v
            end
        end
    end

    return score
end

function parse_input(input::String)
    lines = split(input, "\n\n")

    nums = [parse(Int, v) for v in split(lines[1], ",")]

    boards = BoardState[]
    for grid in lines[2:end]
        b = Vector{Vector{Int}}()
        for line in split(grid, "\n")
            line = replace(line, "  " => " ")
            while line[1] == ' '
                line = line[2:end]
            end
            parts = split(line, " ")

            row = [parse(Int, p) for p in parts]
            push!(b, row)
        end

        push!(boards, BoardState(b))
    end

    return nums, boards
end

function solve(input::String)
    nums, boards = parse_input(input)

    last_winning_score = -1
    already_won = Set{Int}()
    for n in nums
        for (bi, b) in enumerate(boards)
            if bi in already_won
                continue
            end
            did_win = pick_num!(b, n)
            if did_win
                last_winning_score = score(b) * n
                push!(already_won, bi)
            end
        end
    end

    return last_winning_score
end

input = read("input.txt", String)
result = solve(input)
println(result)