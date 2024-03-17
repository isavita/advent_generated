local function is_row_marked(row)
    for _, marked in ipairs(row) do
        if not marked then
            return false
        end
    end
    return true
end

local function is_column_marked(marked, column)
    for _, row in ipairs(marked) do
        if not row[column + 1] then
            return false
        end
    end
    return true
end

local function has_won(board)
    for i = 1, 5 do
        if is_row_marked(board.marked[i]) or is_column_marked(board.marked, i - 1) then
            return true
        end
    end
    return false
end

local function unmarked_sum(board)
    local sum = 0
    for i = 1, 5 do
        for j = 1, 5 do
            if not board.marked[i][j] then
                sum = sum + board.numbers[i][j]
            end
        end
    end
    return sum
end

local function mark(board, number)
    for i = 1, 5 do
        for j = 1, 5 do
            if board.numbers[i][j] == number then
                board.marked[i][j] = true
            end
        end
    end
end

local function read_input()
    local file = io.open("input.txt", "r")
    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()
    return lines
end

local function main()
    local lines = read_input()
    local numbers = {}
    for number in lines[1]:gmatch("%d+") do
        table.insert(numbers, tonumber(number))
    end
    local boards = {}
    for i = 3, #lines, 6 do
        local board = {
            numbers = {},
            marked = {}
        }
        for j = 1, 5 do
            local row = {}
            local marked_row = {}
            for number in lines[i + j - 1]:gmatch("%d+") do
                table.insert(row, tonumber(number))
                table.insert(marked_row, false)
            end
            table.insert(board.numbers, row)
            table.insert(board.marked, marked_row)
        end
        table.insert(boards, board)
    end
    local winning_board, winning_number
    for _, number in ipairs(numbers) do
        for _, board in ipairs(boards) do
            mark(board, number)
            if has_won(board) then
                winning_board = board
                winning_number = number
                break
            end
        end
        if winning_board then
            break
        end
    end
    print(unmarked_sum(winning_board) * winning_number)
end

main()