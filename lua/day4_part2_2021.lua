-- Read the input file
local function read_input(filename)
    local file = io.open(filename, "r")
    local draws = {}
    local boards = {}
    local board = {}

    -- Read the first line for draws
    local line = file:read("*l")
    for number in string.gmatch(line, "%d+") do
        table.insert(draws, tonumber(number))
    end

    -- Read the boards
    local row_count = 0
    for line in file:lines() do
        if line ~= "" then
            row_count = row_count + 1
            local row = {}
            for number in string.gmatch(line, "%d+") do
                table.insert(row, tonumber(number))
            end
            table.insert(board, row)
            if row_count == 5 then
                table.insert(boards, board)
                board = {}
                row_count = 0
            end
        end
    end
    file:close()
    return draws, boards
end

-- Check if a board has won
local function check_win(board_marks)
    -- Check rows
    for i = 1, 5 do
        local row_complete = true
        local col_complete = true
        for j = 1, 5 do
            if not board_marks[i][j] then row_complete = false end
            if not board_marks[j][i] then col_complete = false end
        end
        if row_complete or col_complete then
            return true
        end
    end
    return false
end

-- Calculate the score of the board
local function calculate_score(board, board_marks, last_number)
    local unmarked_sum = 0
    for i = 1, 5 do
        for j = 1, 5 do
            if not board_marks[i][j] then
                unmarked_sum = unmarked_sum + board[i][j]
            end
        end
    end
    return unmarked_sum * last_number
end

-- Play Bingo
local function play_bingo(draws, boards)
    local marks = {}
    local scores = {}
    local winners = {}

    -- Initialize marks
    for i = 1, #boards do
        marks[i] = {}
        for j = 1, 5 do
            marks[i][j] = {}
        end
    end

    -- Draw numbers and mark boards
    for _, number in ipairs(draws) do
        for i, board in ipairs(boards) do
            if not winners[i] then
                for row = 1, 5 do
                    for col = 1, 5 do
                        if board[row][col] == number then
                            marks[i][row][col] = true
                        end
                    end
                end
                if check_win(marks[i]) then
                    winners[i] = true
                    local score = calculate_score(board, marks[i], number)
                    table.insert(scores, score)
                end
            end
        end
    end
    return scores[1], scores[#scores] -- First and last scores
end

-- Main execution
local draws, boards = read_input("input.txt")
local first_winner_score, last_winner_score = play_bingo(draws, boards)
print("Score of the first winning board:", first_winner_score)
print("Score of the last winning board:", last_winner_score)