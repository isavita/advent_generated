
function solve()
    raw = readlines("input.txt")
    raw = filter(!isempty, raw)
    if length(raw) % 7 != 0
        return 0
    end

    locks = Int[]
    keys = Int[]
    for i in 1:7:length(raw)
        block = raw[i:i+6]
        if any(length(ln) < 5 for ln in block)
            continue
        end
        if all(==('#'), block[1])
            push!(locks, parse_lock(block)...)
        else
            push!(keys, parse_key(block)...)
        end
    end

    count = 0
    for i in 1:5:length(locks)
        lock = @view locks[i:i+4]
        for j in 1:5:length(keys)
            key = @view keys[j:j+4]
            if fits(lock, key)
                count += 1
            end
        end
    end
    return count
end

function parse_lock(b)
    h = zeros(Int, 5)
    for c in 1:5
        cnt = 0
        for r in 2:7
            if b[r][c] == '#'
                cnt += 1
            else
                break
            end
        end
        h[c] = cnt
    end
    return h
end

function parse_key(b)
    h = zeros(Int, 5)
    for c in 1:5
        cnt = 0
        for r in 6:-1:1
            if b[r][c] == '#'
                cnt += 1
            else
                break
            end
        end
        h[c] = cnt
    end
    return h
end

fits(lock, key) = all(lock .+ key .<= 5)

println(solve())
