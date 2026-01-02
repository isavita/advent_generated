
function main()
    cur = Int64(50)
    total = Int64(0)
    open("input.txt") do io
        for line in eachline(io)
            isempty(line) && continue
            d = line[1]
            dist = parse(Int64, line[2:end])
            if d == 'R'
                nxt = cur + dist
                total += fld(nxt, 100) - fld(cur, 100)
                cur = nxt
            else
                nxt = cur - dist
                total += fld(cur - 1, 100) - fld(nxt - 1, 100)
                cur = nxt
            end
        end
    end
    println(total)
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
