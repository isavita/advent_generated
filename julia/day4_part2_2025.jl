
function main()
    data = filter(!isempty, readlines("input.txt"))
    if isempty(data)
        println("Total rolls removed: 0")
        return
    end
    rows, cols = length(data), length(first(data))
    grid = [c for line in data for c in collect(line)]
    dirs = (-1:1, -1:1)
    total = 0
    while true
        to_remove = Int[]
        for r = 1:rows, c = 1:cols
            idx = (r - 1) * cols + c
            grid[idx] == '@' || continue
            cnt = 0
            for dr in -1:1, dc in -1:1
                dr == 0 && dc == 0 && continue
                nr, nc = r + dr, c + dc
                (1 ≤ nr ≤ rows && 1 ≤ nc ≤ cols) && (grid[(nr - 1) * cols + nc] == '@') && (cnt += 1)
            end
            cnt < 4 && push!(to_remove, idx)
        end
        isempty(to_remove) && break
        total += length(to_remove)
        for i in to_remove
            grid[i] = '.'
        end
    end
    println("Total rolls removed: $total")
end

main()
