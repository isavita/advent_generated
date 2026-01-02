
function solve_machine(line)
    m = match(r"\[(.*?)\]", line)
    isnothing(m) && return 0
    target = m.captures[1]
    n = length(target)
    b = [c == '#' ? 1 : 0 for c in target]

    btns = Int[]
    for m in eachmatch(r"\((.*?)\)", line)
        v = zeros(Int, n)
        for s in split(m.captures[1], ',')
            s = strip(s)
            isempty(s) && continue
            idx = parse(Int, s)
            0 ≤ idx < n && (v[idx+1] = 1)
        end
        append!(btns, v)
    end
    nb = length(btns) ÷ n
    nb == 0 && return 0

    A = reshape(btns, n, nb)
    A = hcat(A, b)

    pivrow = 0
    pivcol = fill(-1, n)
    for col = 1:nb
        sel = pivrow + 1
        while sel ≤ n && A[sel, col] == 0
            sel += 1
        end
        sel > n && continue
        pivrow += 1
        A[pivrow, :], A[sel, :] = A[sel, :], A[pivrow, :]
        pivcol[pivrow] = col
        for i = 1:n
            i != pivrow && A[i, col] == 1 && (A[i, col:nb+1] .= xor.(A[i, col:nb+1], A[pivrow, col:nb+1]))
        end
    end

    for i = pivrow+1:n
        A[i, nb+1] == 1 && return 0
    end

    ispiv = falses(nb)
    for i = 1:pivrow
        ispiv[pivcol[i]] = true
    end
    free = [j for j = 1:nb if !ispiv[j]]
    nf = length(free)
    best = typemax(Int)

    for mask = 0:(1 << nf) - 1
        x = zeros(Int, nb)
        w = 0
        for (k, v) in enumerate(free)
            (mask >> (k-1)) & 1 == 1 && (x[v] = 1; w += 1)
        end
        for r = 1:pivrow
            val = A[r, nb+1]
            for v in free
                A[r, v] == 1 && x[v] == 1 && (val = xor(val, 1))
            end
            val == 1 && (w += 1)
        end
        w < best && (best = w)
    end
    best == typemax(Int) ? 0 : best
end

function main()
    total = 0
    isfile("input.txt") || return
    for line in eachline("input.txt")
        line = strip(line)
        isempty(line) && continue
        total += solve_machine(line)
    end
    println(total)
end

main()
