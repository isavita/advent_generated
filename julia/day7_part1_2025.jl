
function main()
    data = filter(!isempty, readlines("input.txt"))
    isempty(data) && (println("Empty grid"); return)

    h = length(data)
    w = length(first(data))

    sx = sy = -1
    for y in 1:h
        row = data[y]
        for x in 1:w
            row[x] == 'S' && (sx = x; sy = y; break)
        end
        sx != -1 && break
    end

    active = Set([sx])
    splits = 0

    for y in sy:h
        isempty(active) && break
        nxt = Set{Int}()
        for x in active
            x < 1 || x > w && continue
            c = data[y][x]
            if c == '^'
                splits += 1
                x > 1 && push!(nxt, x - 1)
                x < w && push!(nxt, x + 1)
            else
                push!(nxt, x)
            end
        end
        active = nxt
    end

    println("Total times the beam is split: $splits")
end

main()
