
function solve()
    grid = readlines("input.txt")
    grid = filter(!isempty, grid)
    rows = length(grid)
    cols = length(grid[1])
    word = "XMAS"
    word_len = length(word)
    count = 0
    directions = [(0, 1), (1, 0), (1, 1), (-1, 1), (0, -1), (-1, 0), (-1, -1), (1, -1)]

    for r in 1:rows
        for c in 1:cols
            for (dr, dc) in directions
                valid = true
                for i in 0:word_len-1
                    nr, nc = r + dr * i, c + dc * i
                    if nr < 1 || nr > rows || nc < 1 || nc > cols || grid[nr][nc] != word[i+1]
                        valid = false
                        break
                    end
                end
                if valid
                    count += 1
                end
            end
        end
    end
    println("XMAS appears ", count, " times in the word search")
end

solve()
