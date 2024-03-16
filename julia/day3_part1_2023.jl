function read_file_to_matrix(file_path)
    matrix = []
    open(file_path, "r") do file
        for line in eachline(file)
            push!(matrix, collect(rstrip(line)))
        end
    end
    return matrix
end

function sum_of_part_numbers(matrix)
    sum = 0
    visited = falses(length(matrix), length(matrix[1]))

    for y in 1:length(matrix), x in 1:length(matrix[y])
        if !visited[y, x] && isdigit(matrix[y][x])
            number, length = extract_number(matrix, x, y)
            if is_adjacent_to_symbol(matrix, x, y, length)
                sum += number
            end
            # Mark all digits of this number as visited
            for i in 0:(length-1)
                visited[y, x+i] = true
            end
        end
    end
    return sum
end

function extract_number(matrix, x, y)
    number_str = ""
    while x <= length(matrix[y]) && isdigit(matrix[y][x])
        number_str *= matrix[y][x]
        x += 1
    end
    return parse(Int, number_str), length(number_str)
end

function is_adjacent_to_symbol(matrix, x, y, length)
    for i in 0:(length-1)
        if check_adjacent(matrix, x+i, y)
            return true
        end
    end
    return false
end

function check_adjacent(matrix, x, y)
    for dy in -1:1, dx in -1:1
        adj_x, adj_y = x+dx, y+dy
        if adj_y >= 1 && adj_y <= length(matrix) && adj_x >= 1 && adj_x <= length(matrix[adj_y])
            if !isdigit(matrix[adj_y][adj_x]) && matrix[adj_y][adj_x] != '.'
                return true
            end
        end
    end
    return false
end

matrix = read_file_to_matrix("input.txt")
println(sum_of_part_numbers(matrix))