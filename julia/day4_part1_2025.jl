
function main()
    data = read("input.txt", String) |> strip
    if isempty(data)
        println("Empty grid")
        return
    end
    lines = filter(!isempty, split(data, r"\r?\n"))
    rows, cols = length(lines), length(first(lines))
    grid = [collect(line) for line in lines]
    neigh_offsets = [(dy, dx) for dy in -1:1, dx in -1:1 if !(dy == 0 && dx == 0)]
    count = 0
    @inbounds for y in 1:rows, x in 1:cols
        if grid[y][x] != '@'
            continue
        end
        neigh = 0
        for (dy, dx) in neigh_offsets
            ny, nx = y + dy, x + dx
            if 1 ≤ ny ≤ rows && 1 ≤ nx ≤ cols && grid[ny][nx] == '@'
                neigh += 1
                neigh ≥ 4 && break
            end
        end
        neigh < 4 && (count += 1)
    end
    println("Number of accessible rolls of paper: $count")
end

main()
